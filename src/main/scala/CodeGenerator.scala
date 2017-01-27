/**
  * Created by ftominc on 27/01/17.
  */
class CodeGenerator(map: Map[Identifier, `Type`], declarationZone: DeclarationZone, codeZone: CodeZone) {

  private val header =
    """
      |extern scanf
      |extern printf
      |
      |
      |global main:
      |
      |section .data
      |  FALSE: equ 0
      |  TRUE:  equ 255
      |  _printNum: db "%lf", 0
      |  _printStr: db "%s", 0
      |  _printBool: db "%d", 0
      |section .bss
    """.stripMargin
    def generate = {
      val writer = new Writer("/home/ftominc/Dropbox/compilador/teste2.asm")
      writer.write(header)
      writer.write(varDeclaration(declarationZone.value))
      writer.close
    }

  def generateVarDecl(x: Variable) = {
   val a = map(x.value._2.asInstanceOf[Identifier]) match {
      case NumberType => "resq"
    }
    x.value._1.value + ":" + a
  }

  def getAssemblyCode(head: Token) = {
    head match{
      case x: Variable => generateVarDecl(x)
    }
  }

  def varDeclaration(tokenList: Seq[Token]) = {
      def recur(tokenList: Seq[Token], output: String): String ={
        if(tokenList.isEmpty) output
        else recur(tokenList.tail, output+getAssemblyCode(tokenList.head))
      }
    recur(tokenList, "")
    }
}
