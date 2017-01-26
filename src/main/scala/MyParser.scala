
import fastparse.WhitespaceApi
import fastparse.core.Parser

import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.NumericRange.Inclusive

/**
  * Created by frank on 20/01/2017.
  */
object MyParser {

  def eval(tree: (Token, Seq[(String, Token)])): Token = {
    val (base, ops) = tree
    def recur(tokenList:Seq[(String, Token)], token: Token): Token = {
      if(tokenList.isEmpty) token
      else tokenList.head._1 match {
         case "+" =>  recur(tokenList.tail, Add(token, tokenList.head._2))
         case "-" =>  recur(tokenList.tail, Sub(token, tokenList.head._2))
         case "*" =>  recur(tokenList.tail, Mul(token, tokenList.head._2))
         case "/" =>  recur(tokenList.tail, Div(token, tokenList.head._2))
         case "%" =>  recur(tokenList.tail, Mod(token, tokenList.head._2))
       }
    }
    recur(ops, base)
  }

  def evalLogic(tree: (Token, Seq[(String, Token)])): Token = {
    val (base, ops) = tree
    if(ops.nonEmpty){
        ops.head._1 match {
          case "&&" => And(base, evalLogic(ops.head._2, ops.tail))
          case "||" => Or(base, evalLogic(ops.head._2, ops.tail))
        }
    }
    else base
  }

  def evalRelational(tree: (Token, String, Token)): Token = {
    val (lhs, op, rhs) = tree
    op match {
      case ">"  => Greater(lhs, rhs)
      case "<"  => Less(lhs, rhs)
      case ">=" => GreaterOrEquals(lhs, rhs)
      case "<=" => LessOrEquals(lhs, rhs)
      case "==" => Equals(lhs, rhs)
      case "!=" => NotEquals(lhs, rhs)
    }
  }


  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    val indentation = (0 to 32).map(_.toChar)
    val indentationParser = P(CharIn(indentation).rep)

    NoTrace(indentationParser)
  }

  import fastparse.noApi._
  import White._

  private val reserved = Set(
    "if",
    "else",
    "for",
    "while",
    "write",
    "read",
    "DeclarationZone",
    "CodeZone",
    "switch",
    "case",
    "default",
    "true",
    "false"
  )

  val letter: IndexedSeq[Char]             = ('a' to 'z') ++ ('A' to 'Z')

  val digit: Inclusive[Char]               = '0' to '9'

  val space                                = ' '

  val letterOrDigit: IndexedSeq[Char]      = letter ++ digit

  private val stringAllowed                = space :: letterOrDigit.toList

  val greaterOrEquals = P(">=")

  val lesserOrEquals = P("<=")

  val greater = P(">")

  val lesser = P("<")

  val equals = P("==")

  val notEquals = P("!=")

  val and = P("&&")

  val or = P("||")

  val not = P("!")

  val fractional                           = P( "." ~ P(CharIn(digit).rep))

  val integral                             = P(CharIn(digit).rep)

  private val numberParser                 = P(CharIn("+-").? ~
                                             integral ~
                                             fractional.?).!
                                             .filter(!_.isEmpty)
                                             .map(x=>NumberConst(x.toDouble))

  private val identifierParser             = P(CharIn(letter) ~
                                             CharIn(letterOrDigit).rep(min=0)).!
                                             .filter(!reserved.contains(_))
                                             .map(Identifier)


  private val booleanParser                = P("true" | "false").!
                                             .map(Bool)

  private val stringParser                 = P("\"" ~/ CharIn(stringAllowed).rep ~ "\"").!
                                             .map(StringConst)



  val contentType                          = P( numberParser
                                              | identifierParser
                                              | booleanParser
                                              | stringParser)

  private val varDeclarationToken          = P(identifierParser ~/
                                                "=" ~/
                                                !";" ~
                                                contentType ~
                                                ";")
                                              .map(Variable)

  val parens = P( "(" ~/ addSub ~ ")" )

  val factor = P( numberParser | parens | identifierParser )

  val divMul = P( factor ~ (CharIn("*/%").! ~ factor).rep ).map(eval)

  val addSub: P[Token] = P( divMul ~ !and ~ !or ~ (CharIn("+-").! ~ divMul).rep ).map(eval)

  val relationalOperator = P(greaterOrEquals | lesserOrEquals | greater | lesser | equals)

  val orExpression = P(andExpression ~ ("||".! ~ andExpression).rep).map(evalLogic)

  val andExpression: P[Token] = P(logicOperator ~("&&".! ~ logicOperator).rep).map(evalLogic)

  val notExpression = P(not ~
                      (identifierParser | booleanParser | orExpression)).map(Not)

  val parensLogic = P( "(" ~/ orExpression ~ ")" )

  val logicOperator = P(relationalExpression | notExpression | identifierParser | booleanParser | parensLogic)

 val relationalExpression = P(addSub ~ relationalOperator.! ~ addSub).map(evalRelational)

  val attributionTypes = P(addSub |numberParser | booleanParser | stringParser | identifierParser)

  val attribution = P(identifierParser ~
                      "=" ~/
                      attributionTypes ~
                      ";").map(Attribution)

  val iterationTypes = P(identifierParser | numberParser)

  val iteration = P(identifierParser ~
                    "+=" ~
                    iterationTypes).map(Iteration)

  val read = P("read(" ~
                identifierParser ~
                ");").map(Read)

  val writeTypes = P(identifierParser | stringParser)

  val write = P("write(" ~
                writeTypes ~
                ");").map(Write)

  val switch = P("switch" ~/
                "(" ~
                identifierParser ~
                ")" ~
                "{" ~
                `case`.rep ~
                default ~
                "}").map(Switch)


  val expr   = P(attribution | read | write | switch | `for` | `while`| `if`)

  val `case`: P[Token] = P("case" ~/
                         "(" ~
                         numberParser ~
                         ")" ~
                         "{" ~
                         expr.rep ~
                         "}").map(Case)

  val default: P[Token] = P("default" ~/
                          "{" ~
                          expr.rep ~
                          "}")   .map(Default)

  val `for`:P[Token] = P("for" ~
              "(" ~
              forAttribution ~
              relationalExpression ~
              ";" ~
              iteration ~
              ")" ~
              "{" ~
              expr.rep ~
              "}").map(For)

  val forAttribution = P(identifierParser ~
                       "=" ~
                       (numberParser | identifierParser) ~
                        ";").map(ForAttribution)

  val `while`:P[Token] = P("while" ~/
                  "(" ~
                  orExpression ~
                  ")" ~
                  "{" ~
                  expr.rep ~
                  "}").map(While)

  val `if`: P[Token] = (P("if" ~
                      "(" ~
                      orExpression ~
                      ")" ~
                      "{" ~
                      expr.rep ~
                      "}"
                       ) ~ P("else" ~
                              "{" ~
                              expr.rep ~
                              "}").?).map(If)


  val declarationZone = P("DeclarationZone" ~
                        "{" ~
                         varDeclarationToken.rep ~
                         "}").map(DeclarationZone)

  val codeZone = P("CodeZone" ~
                 "{" ~
                 expr.rep ~
                 "}").map(CodeZone)

  val programParser =                       P(declarationZone ~
                                            codeZone ~
                                            End).map(Program)
}
