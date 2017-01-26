/**
  * Created by frank on 19/01/2017.
  */
sealed trait Token

case class Identifier(value: String) extends Token

case class NumberConst(value: Double) extends Token

case class StringConst(value: String) extends Token

case class BooleanConst(value: String) extends Token

case class Bool(value: String) extends Token

case class Variable(value: (Identifier,Token)) extends Token

trait Exp

case class Add(value: (Token, Token)) extends Token with Exp

case class Sub(value: (Token, Token)) extends Token with Exp

case class Mul(value: (Token, Token)) extends Token with Exp

case class Div(value: (Token, Token)) extends Token with Exp

case class Mod(value:(Token, Token)) extends Token with Exp

case class Attribution(value: (Identifier, Token)) extends Token with Exp

case class Iteration(value: (Identifier, Token)) extends Token with Exp

case class Read(value: Identifier) extends Token with Exp

case class Write(value: Token) extends  Token with Exp

case class Switch(value: (Identifier, Seq[Token], Token)) extends Token with Exp

case class Case(value: (NumberConst, Seq[Token])) extends Token with Exp

case class Default(value: Seq[Token]) extends Token with Exp

case class Not(value: Token) extends Token with Exp

case class And(value: (Token, Token)) extends Token with Exp

case class Or(value: (Token, Token)) extends Token with Exp

case class Greater(value: (Token, Token)) extends Token with Exp

case class Less(value: (Token, Token)) extends Token with Exp

case class GreaterOrEquals(value: (Token, Token)) extends Token with Exp

case class LessOrEquals(value: (Token, Token)) extends Token with Exp

case class Equals(value: (Token, Token)) extends Token with Exp

case class NotEquals(value: (Token, Token)) extends Token with Exp

case class For(value:(Token, Token, Token, Seq[Token])) extends Token with Exp

case class ForAttribution(value:(Token, Token)) extends Token with Exp

case class While(value:(Token, Seq[Token])) extends Token with Exp

case class If(value:(Token, Seq[Token], Option[Seq[Token]])) extends  Token with Exp

case class DeclarationZone(value:(Seq[Token])) extends Token with Exp

case class CodeZone(value:(Seq[Token])) extends Token with Exp

case class Program(value:(DeclarationZone, CodeZone)) extends Token with Exp

trait `Type`

case object NumberType extends `Type`

case object StringType extends `Type`

case object BoolType extends `Type`
