/**
  * Created by ftominc on 27/01/17.
  */
class CodeGenerator(map: Map[Identifier, `Type`], declarationZone: DeclarationZone, codeZone: CodeZone) {
  private var sectionData = ""
  private var numberOfStringConst = 0

  private val header =
    """
      |extern scanf
      |extern printf
      |extern fgets
      |extern stdin
      |extern __fpurge
      |
      |global main:
      |
      |section .data
      |  FALSE: equ 0
      |  TRUE:  equ 1
      |  _printNum: db "%lf",10, 0
      |  _printStr: db "%s",10, 0
      |  _printBool: db "%d",10, 0
      |  _readNum: db "%lf", 0
      |  _readStr: db "%[0-9a-zA-Z' ']", 0
      |  _readByte: db "%c", 0
      |
    """.stripMargin
    private val sectionText =
      """
        |section .text
        |main:
        |  sub rsp, 8                          ;Stack Alignment
      """.stripMargin

    private val exitSection =
      """
        |  add rsp, 8
        |  xor rax, rax
        |  ret
      """.stripMargin
    def generate = {
      val writer = new Writer("D:\\Dropbox\\Dropbox\\compilador\\teste2.asm")
      sectionData = getAssemblyCode(declarationZone.value, buildValueMap(declarationZone.value, Map.empty))
      val codeZoneStr = getAssemblyCode(codeZone.value, buildValueMap(declarationZone.value, Map.empty))
      writer.write(header)
      writer.write(sectionData)
      writer.write(sectionText)
      writer.write(codeZoneStr)
      writer.write(exitSection)
      writer.close
    }

  def generateVarDecl(x: Variable, valueMap: Map[Identifier, Token]) = {
   val a = map.get(x.value._1) match {
      case Some(NumberType) => "dq "
      case Some(StringType) => "db "
      case Some(BoolType) => "db "
      case _ => "db "
    }
    val b = x.value._2 match {
      case NumberConst(n) => n
      case StringConst(n) => n + ",0\r\n times 255-$+" + x.value._1.value + " db 0"
      case Bool("true") => "TRUE"
      case Bool("false") => "FALSE"
      case n:Identifier => valueMap(n) match{
        case NumberConst(k) => k
        case StringConst(k) => k + ",0\r\n times 255-$+" + x.value._1.value + " db 0"
        case Bool("true") => "TRUE"
        case Bool("false") => "FALSE"
      }
    }
    x.value._1.value + ":" + a + b + "\r\n"
  }

  def generateAttribution(x: Attribution) = {
    def recurr(x: Token):String ={
      x match {
        case Add(y) => recurr(y._1) + recurr(y._2) +
                       """
                         |movsd xmm1, [rsp]
                         |add rsp, 8
                         |movsd xmm0, [rsp]
                         |add rsp, 8
                         |addsd XMM0, XMM1
                         |sub rsp, 8
                         |movsd [rsp], XMM0
                         |
                       """.stripMargin
        case Sub(y) => recurr(y._1) + recurr(y._2) +
                       """
                         |movsd xmm1, [rsp]
                         |add rsp, 8
                         |movsd xmm0, [rsp]
                         |add rsp, 8
                         |subsd XMM0, XMM1
                         |sub rsp, 8
                         |movsd [rsp], XMM0
                         |
                       """.stripMargin
        case Div(y) =>  recurr(y._1) + recurr(y._2) +
                        """
                          |movsd xmm1, [rsp]
                          |add rsp, 8
                          |movsd xmm0, [rsp]
                          |add rsp, 8
                          |divsd XMM0, XMM1
                          |sub rsp, 8
                          |movsd [rsp], XMM0
                          |
                        """.stripMargin
        case Mul(y) => recurr(y._1) + recurr(y._2) +
                      """
                        |movsd xmm1, [rsp]
                        |add rsp, 8
                        |movsd xmm0, [rsp]
                        |add rsp, 8
                        |mulsd XMM0, XMM1
                        |sub rsp, 8
                        |movsd [rsp], XMM0
                        |
                      """.stripMargin
        case Mod(y) => recurr(y._1) + recurr(y._2) +
                       """
                         |movsd xmm0, [rsp]
                         |add rsp, 8
                         |movsd  xmm1, [rsp]
                         |add rsp, 8
                         |mov rax, 2
                         |call fmod
                         |sub rsp, 8
                         |movsd [rsp], rax
                         |
                       """.stripMargin
        case Identifier(y) =>
          s"""
            |push qword [$y]
          """.stripMargin

        case NumberConst(y) =>
          s"""
            |mov rax, 0x${java.lang.Double.doubleToLongBits(y).toHexString}
            |push rax
          """.stripMargin

      }
    }
    x.value._2 match{
      case y: Add => recurr(y)+
                     s"""
                       |movsd xmm0, [rsp]
                       |add rsp, 8
                       |movsd [${x.value._1.value}], xmm0
                     """.stripMargin
      case y: Sub => recurr(y)+
                     s"""
                       |movsd xmm0, [rsp]
                       |add rsp, 8
                       |movsd [${x.value._1.value}], xmm0
                     """.stripMargin
      case y: Mul => recurr(y)+
                     s"""
                       |movsd xmm0, [rsp]
                       |add rsp, 8
                       |movsd [${x.value._1.value}], xmm0
                     """.stripMargin
      case y: Div => recurr(y)+
                     s"""
                       |movsd xmm0, [rsp]
                       |add rsp, 8
                       |movsd [${x.value._1.value}], xmm0
                     """.stripMargin
      case y: Mod => recurr(y)+
                     s"""
                       |movsd xmm0, [rsp]
                       |add rsp, 8
                       |movsd [${x.value._1.value}], xmm0
                     """.stripMargin
      case y: Identifier => map(x.value._1) match {
        case NumberType =>    s"""
                              |movsd xmm0, [${y.value}]
                              |movsd [${x.value._1.value}], xmm0
                              """.stripMargin

        case StringType =>    s"""
                              |cld
                              |mov rsi, ${y.value}
                              |mov rdi, ${x.value._1.value}
                              |mov rcx, 255
                              |rep movsb
                              """.stripMargin
        case BoolType =>
          s"""
             |mov al, byte [${y.value}]
             |mov [${x.value._1.value}], byte al
           """.stripMargin
      }
      case y: NumberConst => s"""
          |mov rax, 0x${java.lang.Double.doubleToLongBits(y.value).toHexString}
          |push rax
        """.stripMargin
      case Bool(y) => y match{
        case "true" =>
          s"""
            |mov [${x.value._1.value}], byte TRUE
          """.stripMargin
        case "false" => {
          s"""
             |mov [${x.value._1.value}], byte FALSE
          """.stripMargin
        }
      }
      case StringConst(y) =>
        val map2: Map[Identifier,Token] = Map.empty + (Identifier("_stringConst" + numberOfStringConst) -> StringConst(y))
        sectionData = sectionData + generateVarDecl(Variable(Identifier("_stringConst" + numberOfStringConst), StringConst(y)),map2)
        val b =
          s"""
             |cld
             |mov rsi, _stringConst${numberOfStringConst}
             |mov rdi, ${x.value._1.value}
             |mov rcx, 255
             |rep movsb
           """.stripMargin
        .stripMargin
        numberOfStringConst = numberOfStringConst + 1
        b
    }
  }

  def generateWrite(x: Write) = {
    x.value match {
      case k: Identifier => map(k) match {
        case NumberType =>
          s"""
            |mov rax, 1
            |movsd xmm0, [${k.value}]
            |mov rdi, _printNum
            |call printf
          """.stripMargin
        case StringType =>
          s"""
             |xor rax, rax
             |mov rdi, _printStr
             |mov rsi, ${k.value}
             |call printf
           """.stripMargin
        case BoolType =>
          s"""
             |xor rax, rax
             |mov rdi, _printBool
             |movzx esi, byte [${k.value}]
             |call printf
           """.stripMargin
      }
      case k: StringConst => val map2: Map[Identifier,Token] = Map.empty + (Identifier("_stringConst" + numberOfStringConst) -> k)
        sectionData = sectionData + generateVarDecl(Variable(Identifier("_stringConst" + numberOfStringConst), k),map2)
        val b =
          s"""
             |xor rax, rax
             |mov rdi, _printStr
             |mov rsi, _stringConst$numberOfStringConst
             |call printf
           """.stripMargin
        numberOfStringConst = numberOfStringConst + 1
        b
    }
  }

  def generateRead(x: Read) = {
    map(x.value) match {
      case NumberType =>
        s"""
           |  mov rdi, _readNum
           |  mov rsi, ${x.value.value}
           |  xor rax, rax
           |  call scanf
         """.stripMargin
      case StringType =>
        s"""
           |  mov rdi, ${x.value.value}
           |  mov rsi, 255
           |  mov rdx, [stdin]
           |  call fgets
           |  mov rdi, [stdin]
           |  call __fpurge
         """.stripMargin
      case BoolType =>
        s"""
           |  mov rdi, _readByte
           |  mov rsi, ${x.value.value}
           |  xor rax, rax
           |  call scanf
           |  sub [${x.value.value}], 48d
         """.stripMargin
    }
  }

  def generateFor(x: For) = {
    x.value._1 match {
      case ForAttribution(k) =>
        s"""
          |mov [${k._1}
        """.stripMargin
    }
  }

  def getAssemblyCode(tokenList: Seq[Token], map2: Map[Identifier, Token]) = {
    println(tokenList)
    def recurr(tokenList: Seq[Token], code:String): String ={
      if(tokenList.isEmpty) code
      else {
        val x = tokenList.head match {
          case x: Variable => generateVarDecl(x, map2)
          case x: Attribution => generateAttribution(x)
          case x: Write => generateWrite(x)
          case x: Read => generateRead(x)
          case x: For => generateFor(x)
        }
        recurr(tokenList.tail, code + x)
      }
    }
    recurr(tokenList, "")
  }

  def buildValueMap(tokenList: Seq[Token], map: Map[Identifier, Token]): Map[Identifier, Token] = {
    if(tokenList.isEmpty) map
    else {
      val b = tokenList.head match {
        case Variable(x) => (x._1,x._2)
        case _ => (Identifier("_"),NumberConst(0))
      }
      buildValueMap(tokenList.tail, map + b)
    }
  }
}
