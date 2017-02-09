/**
  * Created by ftominc on 27/01/17.
  */
class CodeGenerator(map: Map[Identifier, `Type`], declarationZone: DeclarationZone, codeZone: CodeZone) {
  private var sectionData = ""
  private var numberOfStringConst = 0
  private var forCount = 0
  private var switchCount = 0
  private var whileCount = 0
  private var ifCount = 0
  private var readBooleanCount = 0

  private val header =
    """
      |extern scanf
      |extern printf
      |extern fgets
      |extern stdin
      |extern __fpurge
      |extern fmod
      |
      |global main:
      |
      |section .data
      |  FALSE: equ 0
      |  TRUE:  equ 255
      |  _printNum: db "%lf",10, 0
      |  _printStr: db "%s",10, 0
      |  _printBool: db "%d",10, 0
      |  _readNum: db "%lf", 0
      |  _readStr: db "%[0-9a-zA-Z' ']", 0
      |  _readByte: db "%d", 0
      |  _tmp: dq 0
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

  def generate(out: String) = {
    val writer = new Writer(out)
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
      case Some(BoolType) => "dq "
      case _ => "db "
    }
    val b = x.value._2 match {
      case NumberConst(n) => n
      case StringConst(n) => n + ",0\r\n times 255-$+" + x.value._1.value + " db 0"
      case Bool("true") => "TRUE"
      case Bool("false") => "FALSE"
      case n: Identifier => valueMap(n) match {
        case NumberConst(k) => k
        case StringConst(k) => k + ",0\r\n times 255-$+" + x.value._1.value + " db 0"
        case Bool("true") => "TRUE"
        case Bool("false") => "FALSE"
      }
    }
    x.value._1.value + ":" + a + b + "\r\n"
  }

  def generateAttribution(x: Attribution) = {
    println("Generating Attribution for " + x)
    x.value._2 match {
      case y: Identifier => map(x.value._1) match {
        case NumberType => s"""
                              |movsd xmm0, [${y.value}]
                              |movsd [${x.value._1.value}], xmm0
                              """.stripMargin

        case StringType => s"""
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

        case _ => getAssemblyCode(List(y), Map.empty)
      }
      case y: NumberConst => s"""
                                |mov rax, 0x${java.lang.Double.doubleToLongBits(y.value).toHexString}
                                |mov [${x.value._1.value}], rax
        """.stripMargin
      case Bool(y) => y match {
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
        val map2: Map[Identifier, Token] = Map.empty + (Identifier("_stringConst" + numberOfStringConst) -> StringConst(y))
        sectionData = sectionData + generateVarDecl(Variable(Identifier("_stringConst" + numberOfStringConst), StringConst(y)), map2)
        val b =
          s"""
             |cld
             |mov rsi, _stringConst$numberOfStringConst
             |mov rdi, ${x.value._1.value}
             |mov rcx, 255
             |rep movsb
           """.stripMargin
            .stripMargin
        numberOfStringConst = numberOfStringConst + 1
        b
      case y: Add => generateMath(y) +
        s"""
           |  movsd xmm0, [rsp]
           |  add rsp, 8
           |  movsd [${x.value._1.value}], xmm0
           |""".stripMargin
      case y: Sub => generateMath(y) +
        s"""
           |  movsd xmm0, [rsp]
           |  add rsp, 8
           |  movsd [${x.value._1.value}], xmm0
           |""".stripMargin
      case y: Mul => generateMath(y) +
        s"""
           |  movsd xmm0, [rsp]
           |  add rsp, 8
           |  movsd [${x.value._1.value}], xmm0
           |""".stripMargin
      case y: Div => generateMath(y) +
        s"""
           |  movsd xmm0, [rsp]
           |  add rsp, 8
           |  movsd [${x.value._1.value}], xmm0
           |""".stripMargin
      case y: Mod => generateMath(y) +
        s"""
           |  movsd xmm0, [rsp]
           |  add rsp, 8
           |  movsd [${x.value._1.value}], xmm0
           |""".stripMargin
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
             |mov esi, [${k.value}]
             |call printf
           """.stripMargin
      }
      case k: StringConst => val map2: Map[Identifier, Token] = Map.empty + (Identifier("_stringConst" + numberOfStringConst) -> k)
        sectionData = sectionData + generateVarDecl(Variable(Identifier("_stringConst" + numberOfStringConst), k), map2)
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
        readBooleanCount = readBooleanCount + 1
        s"""
           |  mov rdi, _readByte
           |  mov rsi, ${x.value.value}
           |  xor rax, rax
           |  call scanf
           |  sub [${x.value.value}], 48d
           |  cmp [${x.value.value}], 0
           |  je FALSE_LABEL$readBooleanCount
           |  or [${x.value.value}], 0xffffffffffffffff
           |  jump TRUE_LABEL$readBooleanCount
           |  FALSE_LABEL$readBooleanCount:
           |  and [${x.value.value}], 0x0000000000000000
           |  TRUE_LABEL$readBooleanCount:
         """.stripMargin
    }
  }

  def generateFor(x: For) = {
    val enter = s"enterFor$forCount"
    val leave = s"leaveFor$forCount"
    forCount = forCount + 1
    val a = x.value._1 match {
      case ForAttribution(k) =>
        k._2 match {
          case NumberConst(m) =>
            s"""
               |  mov rax, 0x${java.lang.Double.doubleToLongBits(m).toHexString}
               |  push rax
               |  movsd xmm0, [rsp]
               |  add rsp, 8
               |  movsd [${k._1.asInstanceOf[Identifier].value}], xmm0
        """.stripMargin

          case Identifier(m) =>
            s"""
               |movsd xmm0, [$m]
               |movsd [${k._1.asInstanceOf[Identifier].value}], xmm0
             """.stripMargin
        }
    }
    val b = x.value._2 match {
      case y: Less =>
        s"$enter:" +
          getAssemblyCode(List(y.value._1), Map.empty) + getAssemblyCode(List(y.value._2), Map.empty) +
          s"""
             |movsd xmm0, [rsp]
             |add rsp, 8
             |movsd xmm1, [rsp]
             |add rsp, 8
             |ucomisd xmm1, xmm0
             |jnc $leave
            """.stripMargin +
          getAssemblyCode(x.value._4, Map.empty) +
          getAssemblyCode(List(x.value._3), Map.empty) +
          s"""
             |jmp $enter
             |$leave:
          """.stripMargin


      case y: LessOrEquals => s"$enter:" +
        getAssemblyCode(List(y.value._1), Map.empty) + getAssemblyCode(List(y.value._2), Map.empty) +
        s"""
           |movsd xmm0, [rsp]
           |add rsp, 8
           |movsd xmm1, [rsp]
           |add rsp, 8
           |ucomisd xmm1, xmm0
           |ja $leave
            """.stripMargin +
        getAssemblyCode(x.value._4, Map.empty) +
        getAssemblyCode(List(x.value._3), Map.empty) +
        s"""
           |jmp $enter
           |$leave:
          """.stripMargin

      case y: Greater => s"$enter:" +
        getAssemblyCode(List(y.value._1), Map.empty) + getAssemblyCode(List(y.value._2), Map.empty) +
        s"""
           |movsd xmm0, [rsp]
           |add rsp, 8
           |movsd xmm1, [rsp]
           |add rsp, 8
           |ucomisd xmm1, xmm0
           |jbe $leave
            """.stripMargin +
        getAssemblyCode(x.value._4, Map.empty) +
        getAssemblyCode(List(x.value._3), Map.empty) +
        s"""
           |jmp $enter
           |$leave:
          """.stripMargin
      case y: GreaterOrEquals => s"$enter:" +
        getAssemblyCode(List(y.value._1), Map.empty) + getAssemblyCode(List(y.value._2), Map.empty) +
        s"""
           |movsd xmm0, [rsp]
           |add rsp, 8
           |movsd xmm1, [rsp]
           |add rsp, 8
           |ucomisd xmm1, xmm0
           |jc $leave
            """.stripMargin +
        getAssemblyCode(x.value._4, Map.empty) +
        getAssemblyCode(List(x.value._3), Map.empty) +
        s"""
           |jmp $enter
           |$leave:
          """.stripMargin
      case y: Equals => s"$enter:" +
        getAssemblyCode(List(y.value._1), Map.empty) + getAssemblyCode(List(y.value._2), Map.empty) +
        s"""
           |movsd xmm0, [rsp]
           |add rsp, 8
           |movsd xmm1, [rsp]
           |add rsp, 8
           |ucomisd xmm1, xmm0
           |jnz $leave
            """.stripMargin +
        getAssemblyCode(x.value._4, Map.empty) +
        getAssemblyCode(List(x.value._3), Map.empty) +
        s"""
           |jmp $enter
           |$leave:
          """.stripMargin
      case y: NotEquals => s"$enter:" +
        getAssemblyCode(List(y.value._1), Map.empty) + getAssemblyCode(List(y.value._2), Map.empty) +
        s"""
           |movsd xmm0, [rsp]
           |add rsp, 8
           |movsd xmm1, [rsp]
           |add rsp, 8
           |ucomisd xmm1, xmm0
           |jz $leave
            """.stripMargin +
        getAssemblyCode(x.value._4, Map.empty) +
        getAssemblyCode(List(x.value._3), Map.empty) +
        s"""
           |jmp $enter
           |$leave:
          """.stripMargin
    }

    a + b
  }

  def generateMath(x: Token) = {
    def recur(x: Token): String = {
      x match {
        case Add(y) => recur(y._1) + recur(y._2) +
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
        case Sub(y) => recur(y._1) + recur(y._2) +
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
        case Div(y) => recur(y._1) + recur(y._2) +
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
        case Mul(y) => recur(y._1) + recur(y._2) +
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
        case Mod(y) => recur(y._1) + recur(y._2) +
          """
            |movsd xmm0, [rsp]
            |add rsp, 8
            |movsd  xmm1, [rsp]
            |add rsp, 8
            |mov rax, 2
            |call fmod
            |sub rsp, 8
            |mov [rsp], rax
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

    recur(x)
  }

  def pushIdentifier(x: Identifier) =
    s"""
       |push qword [${x.value}]
    """.stripMargin

  def pushNumberConst(x: NumberConst) =
    s"""
       |mov rax, 0x${java.lang.Double.doubleToLongBits(x.value).toHexString}
       |push rax
    """.stripMargin

  def generateIteration(x: Iteration) = {
    val a = x.value._2 match {
      case Identifier(y) =>
        s"""
           |movsd xmm2, [${x.value._1.value}]
           |movsd xmm3. [$y]
         """.stripMargin
      case NumberConst(y) =>
        s"""
           |movsd xmm2, [${x.value._1.value}]
           |mov rax, 0x${java.lang.Double.doubleToLongBits(y).toHexString}
           |push rax
           |movsd xmm3, [rsp]
           |add rsp, 8
         """.stripMargin
    }
    a +
      s"""
         |addsd xmm2, xmm3
         |movsd [${x.value._1.value}], xmm2
       """.stripMargin
  }

  def generateSwitch(x: Switch) = {
    val startcase = s"case${switchCount}"
    val endSwitch = s"endSwitch${switchCount}"
    switchCount = switchCount + 1
    val a =
      s"""
         |movsd xmm0, [${x.value._1.value}]
      """.stripMargin

    def recur(tokenList: Seq[Token], jumpSection: String, codeSection: String): (String, String) = {
      if (tokenList.isEmpty) (jumpSection, codeSection + s"$endSwitch:")
      else {
        tokenList.head match {
          case x: Case => recur(tokenList.tail,
            jumpSection +
              s"""
                 |mov rax, 0x${java.lang.Double.doubleToLongBits(x.value._1.value).toHexString}
                 |push rax
                 |movsd xmm1, [rsp]
                 |add rsp, 8
                 |ucomisd xmm0, xmm1
                 |je $startcase${x.value._1.value}
                                   """.stripMargin,
            codeSection +
              s"""
                 |$startcase${x.value._1.value}:
                 |
                                   """.stripMargin +
              getAssemblyCode(x.value._2, Map.empty) +
              s"""
                 | jmp $endSwitch
                                  """.stripMargin)
        }
      }
    }

    val c = recur(x.value._2, "", "")
    a + c._1 + getAssemblyCode(x.value._3.asInstanceOf[Default].value, Map.empty) +
      s"""
         |jmp $endSwitch
         |""".stripMargin + c._2
  }

  def generateWhile(x: While) = {
    val enter = s"while$whileCount"
    val leave = s"endWhile$whileCount"
    whileCount = whileCount + 1
    val relationalHeader =
      s"""
         |movsd xmm0, [rsp]
         |add rsp, 8
         |movsd xmm1, [rsp]
         |add rsp, 8
         |ucomisd xmm1, xmm0
      """.stripMargin
    x.value._1 match {
      case y: Less => enter + ":" + getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jnc $leave
                        """.stripMargin + getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
           |$leave:
                       """.stripMargin
      case y: LessOrEquals => enter + ":" + getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |ja $leave
                                      """.stripMargin + getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
           |$leave:
                                     """.stripMargin
      case y: Greater => enter + ":" + getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jbe $leave
                                    """.stripMargin + getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
           |$leave:
                                   """.stripMargin
      case y: GreaterOrEquals => enter + ":" + getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jc $leave
                                    """.stripMargin + getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
           |$leave:
                                   """.stripMargin
      case y: Equals => enter + ":" + getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jnz $leave
                                    """.stripMargin + getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
           |$leave:
                                   """.stripMargin
      case y: NotEquals => enter + ":" + getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jz $leave
                                    """.stripMargin + getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
           |$leave:
                                   """.stripMargin
      case y: And =>
        s"$enter:"+
        generateLogical(y) +
        s"""
           |pop rax
           |test rax, rax
           |jz $leave
        """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
        """.stripMargin +
        leave + ":"
      case y: Or => s"$enter:"+
        generateLogical(y) +
        s"""
           |pop rax
           |test rax, rax
           |jz $leave
        """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
        """.stripMargin +
        leave + ":"
      case y: Not => s"$enter:"+
        generateLogical(y) +
        s"""
           |pop rax
           |test rax, rax
           |jz $leave
        """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        s"""
           |jmp $enter
        """.stripMargin +
        leave + ":"
      case y: Bool => y.value match {
        case "true" => s"$enter" + getAssemblyCode(x.value._2, Map.empty) + s"jmp $enter"
        case "false" => ""
      }
      case y: Identifier =>
        s"""
           |$enter:
           |mov rax, [${y.value}]
           |test rax, rax
           |jz $leave
         """.stripMargin +
          getAssemblyCode(x.value._2, Map.empty)+
          s"""
             |jmp $enter
          """.stripMargin +
          leave + ":"
    }
  }

  def generateIf(x: If) = {
    val enter = s"if$ifCount"
    val leave = s"endif$ifCount"
    ifCount = ifCount + 1
    val relationalHeader =
      s"""
         |movsd xmm0, [rsp]
         |add rsp, 8
         |movsd xmm1, [rsp]
         |add rsp, 8
         |ucomisd xmm1, xmm0
      """.stripMargin
    val elseCode = x.value._3 match {
      case Some(k) => getAssemblyCode(k, Map.empty)
      case _ => ""
    }
    x.value._1 match {
      case y: Less => getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jc $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
         """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        leave + ":"
      case y: LessOrEquals => getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jbe $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
         """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        leave + ":"
      case y: Greater => getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |ja $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
         """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        leave + ":"
      case y: GreaterOrEquals => getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jae $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
         """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        leave + ":"
      case y: Equals => getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |je $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
         """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        leave + ":"
      case y: NotEquals => getAssemblyCode(List(y.value._1), Map.empty) +
        getAssemblyCode(List(y.value._2), Map.empty) +
        relationalHeader +
        s"""
           |jne $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
         """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty) +
        leave + ":"
      case y: And => generateLogical(y) +
        s"""
           |pop rax
           |test rax, rax
           |jnz $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
                      """.stripMargin +
          getAssemblyCode(x.value._2, Map.empty)+
          leave + ":"
      case y: Or => generateLogical(y)+
        s"""
           |pop rax
           |test rax, rax
           |jnz $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
                      """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty)+
        leave + ":"
      case y: Not => generateLogical(y)+
        s"""
           |pop rax
           |test rax, rax
           |jnz $enter
        """.stripMargin +
        elseCode +
        s"""
           |jmp $leave
           |$enter:
                      """.stripMargin +
        getAssemblyCode(x.value._2, Map.empty)+
        leave + ":"
      case y: Bool => y.value match {
        case "true" => getAssemblyCode(x.value._2, Map.empty)
        case "false" => elseCode
      }
      case y: Identifier =>
        s"""
           |mov rax, [${y.value}]
           |test rax, rax
           |jnz $enter
         """.stripMargin +
          elseCode +
          s"""
             |jmp $leave
             |$enter:
                      """.stripMargin +
          getAssemblyCode(x.value._2, Map.empty)+
          leave + ":"
    }
  }

  def generateLogical(x: Token) = {
    val relationalHeader =
      s"""
         |xor rax, rax
         |movsd xmm0, [rsp]
         |add rsp, 8
         |movsd xmm1, [rsp]
         |add rsp, 8
         |ucomisd xmm1, xmm0
      """.stripMargin

    val logicalHeader =
      s"""
         |pop rax
         |pop rbx
       """.stripMargin
    def recur(token: Token): String = {
      token match {
        case x: Identifier =>
          s"""
             |push qword [${x.value}]
           """.stripMargin
        case x: NumberConst =>
          s"""
             |mov rax, 0x${java.lang.Double.doubleToLongBits(x.value).toHexString}
             |push rax
          """.stripMargin
        case x: Less => recur(x.value._1) +
          recur(x.value._2) +
          relationalHeader +
          s"""
             |SETB al
             |push rax
           """.stripMargin
        case x: LessOrEquals => recur(x.value._1) +
          recur(x.value._2) +
          relationalHeader +
          s"""
             |SETBE al
             |push rax
           """.stripMargin
        case x: Greater => recur(x.value._1) +
          recur(x.value._2) +
          relationalHeader +
          s"""
             |setg al
             |push rax
           """.stripMargin
        case x: GreaterOrEquals => recur(x.value._1) +
          recur(x.value._2) +
          relationalHeader +
          s"""
             |setge al
             |push rax
          """.stripMargin
        case x: Equals => getAssemblyCode(List(x.value._1),Map.empty) +
          getAssemblyCode(List(x.value._2), Map.empty) +
          relationalHeader +
          s"""
             |sete al
             |push rax
          """.stripMargin
        case x: NotEquals => recur(x.value._1)+
          recur(x.value._2) +
          relationalHeader +
          s"""
             |setne al
             |push rax
          """.stripMargin
        case x: And => recur(x.value._1) +
          recur(x.value._2) +
          logicalHeader +
          s"""
             |and rax, rbx
             |push rax
          """.stripMargin
        case x: Or => recur(x.value._1) +
          recur(x.value._2) +
          logicalHeader +
          s"""
             |or rax, rbx
             |push rax
          """.stripMargin
        case x: Not =>
          x.value match {
            case k: Identifier =>
              s"""
                 |not byte [${k.value}]
                 |and byte [${k.value}], 255d
                 |push qword [${k.value}]
               """.stripMargin
            case k: BooleanConst =>
              s"""
                 |mov rax, ${k.value.toUpperCase}
                 |not rax
                 |and rax, 255d
                 |push rax
               """.stripMargin
          }
        case x: Bool => x.value match {
          case "true" =>
            s"""
               |xor rax, rax
               |mov al, TRUE
               |push rax
             """.stripMargin
          case "false" =>
            s"""
               |xor rax, rax
               |mov al, FALSE
               |push rax
             """.stripMargin
        }
      }
    }

    recur(x)
  }

  def getAssemblyCode(tokenList: Seq[Token], map2: Map[Identifier, Token]): String = {
    println(tokenList)

    def recurr(tokenList: Seq[Token], code: String): String = {
      if (tokenList.isEmpty) code
      else {
        val x = tokenList.head match {
          case x: Variable => generateVarDecl(x, map2)
          case x: Attribution => generateAttribution(x)
          case x: Write => generateWrite(x)
          case x: Read => generateRead(x)
          case x: For => generateFor(x)
          case x: Add => generateMath(x)
          case x: Sub => generateMath(x)
          case x: Div => generateMath(x)
          case x: Mod => generateMath(x)
          case x: Mul => generateMath(x)
          case x: Identifier => pushIdentifier(x)
          case x: NumberConst => pushNumberConst(x)
          case x: Iteration => generateIteration(x)
          case x: Switch => generateSwitch(x)
          case x: While => generateWhile(x)
          case x: If => generateIf(x)
        }
        recurr(tokenList.tail, code + x)
      }
    }

    recurr(tokenList, "")
  }

  def buildValueMap(tokenList: Seq[Token], map: Map[Identifier, Token]): Map[Identifier, Token] = {
    if (tokenList.isEmpty) map
    else {
      val b = tokenList.head match {
        case Variable(x) => (x._1, x._2)
        case _ => (Identifier("_"), NumberConst(0))
      }
      buildValueMap(tokenList.tail, map + b)
    }
  }
}
