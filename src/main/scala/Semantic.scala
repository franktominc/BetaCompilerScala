/**
  * Created by frank on 23/01/2017.
  */



object Semantic {

  trait Status

  case object Success extends Status

  def evalFor(x: (Token, Token, Token, Seq[Token]), map: Map[Identifier, `Type`]): Either[String,Status] = {
    def recur(list: Seq[Token], either: Either[String,Status]): Either[String, Status] ={
      if(list.isEmpty) either
      else recur(list.tail, eval(list.head, map))
    }
    eval(x._1, map) match {
      case Left(e) => return Left(e)
      case _ => Right(Success)
    }
    eval(x._2, map) match {
      case Left(e) => return Left(e)
      case _ => Right(Success)
    }
    eval(x._3, map)  match {
      case Left(e) => return Left(e)
      case _ => Right(Success)
    }
    recur(x._4, Right(Success))
  }

  def evalForAttribution(x: (Token, Token), map: Map[Identifier, `Type`]) = {

    val a = map.get(x._1.asInstanceOf[Identifier])

    val b: Either[String, Status] = a match{
      case Some(x) => x match {
        case NumberType => Right(Success)
        case _ => Left("Tipo da variavel incorreto")
      }
      case None => Left("Variavel nao foi declarada")
    }

    val c: Either[String, Status] = x._2 match {
      case NumberConst(_) => Right(Success)
      case Identifier(x) => map.get(x.asInstanceOf[Identifier]) match{
        case Some(NumberType) => Right(Success)
        case _ => Left("Expressao invalida")
      }
    }

    if(b.equals(c))
      Right(Success)
    else if(b.isLeft) b
    else c
  }

  def evalIteration(x: (Identifier, Token), map: Map[Identifier, `Type`]): Either[String, Status] = {
    val a = map.get(x._1)

    val b: Either[String, Status] = a match{
      case Some(x) => x match {
        case NumberType => Right(Success)
        case _ => Left("Tipo da variavel nao eh um number")
      }
      case None => Left("Variavel nao foi declarada")
    }

    val c: Either[String, Status] = x._2 match {
      case NumberConst(_) => Right(Success)
      case Identifier(x) => map.get(x.asInstanceOf[Identifier]) match{
      case Some(NumberType) => Right(Success)
      case _ => Left("Expressao invalida")
      }
    }

    if(b.equals(c))
      Right(Success)
    else if(b.isLeft) b
    else c

  }

  def evalSwitch(x: (Identifier, Seq[Token], Token), map: Map[Identifier, `Type`]): Either[String, Status] = {
    def recur(list: Seq[Token], either: Either[String,Status]): Either[String, Status] ={
      if(list.isEmpty) either
      else recur(list.tail, eval(list.head, map))
    }
    val a = map.get(x._1)

    val b = a match {
      case Some(x) => x match {
      case NumberType => Right(Success)
      case _ => Left("Tipo da variavel nao eh um number")
      }
      case None => Left("Variavel nao foi declarada")
    }

    val c = recur(x._2, Right(Success))

    if(b.equals(c))
      Right(Success)
    else if(b.isLeft) b
    else c

  }

  def evalWrite(x: Token, map: Map[Identifier, `Type`]): Either[String, Success.type] = {

    x match {
      case StringConst(_) => Right(Success)
      case x: Identifier => map.get(x) match {
        case Some(_) => Right(Success)
        case _ => Left("Variavel nao declarada")
      }
    }
  }

  def evalCase(x: (NumberConst, Seq[Token]), map: Map[Identifier, `Type`]): Either[String, Status] = {
    def recur(list: Seq[Token], either: Either[String,Status]): Either[String, Status] ={
      if(list.isEmpty) either
      else recur(list.tail, eval(list.head, map))
    }
    recur(x._2, Right(Success))
  }

  def evalMath(x: (Token, Token), map: Map[Identifier, `Type`]): Either[String, Status] = {
    def evalMathP2(x: Token, map: Map[Identifier, `Type`]): Either[String, Status] = {
      x match {
        case Add(_) => eval(x, map) match {
          case Right(_) => Right(Success)
          case Left(_) => Left("Shit happened")
        }
        case Sub(_) => eval(x, map) match {
          case Right(_) => Right(Success)
          case Left(_) => Left("Shit happened")
        }
        case Mul(_) => eval(x, map) match {
          case Right(_) => Right(Success)
          case Left(_) => Left("Shit happened")
        }
        case Div(_) => eval(x, map) match {
          case Right(_) => Right(Success)
          case Left(_) => Left("Shit happened")
        }
        case Mod(_) => eval(x, map) match {
          case Right(_) => Right(Success)
          case Left(_) => Left("Shit happened")
        }
        case a: Identifier => map.get(a) match {
          case Some(k) => k match {
            case NumberType => Right(Success)
            case _ => Left("Wrong Type")
          }
          case _ => Left("Variavel nao declarada")
        }
        case NumberConst(_) => Right(Success)
        case _ => Left("Tipo Incompativel Encontrado")
      }

    }
    x._1 match {
      case Add(_) => eval(x._1, map) match {
        case Right(_) => evalMathP2(x._2, map)
        case Left(_) => Left("Oops, Somehow somewhere something went wrong")
      }
      case Sub(_) => eval(x._1, map)match {
        case Right(_) => evalMathP2(x._2, map)
        case Left(_) => Left("Oops, Somehow somewhere something went wrong")
      }
      case Mul(_) => eval(x._1, map)match {
        case Right(_) => evalMathP2(x._2, map)
        case Left(_) => Left("Oops, Somehow somewhere something went wrong")
      }
      case Div(_) => eval(x._1, map)match {
        case Right(_) => evalMathP2(x._2, map)
        case Left(_) => Left("Oops, Somehow somewhere something went wrong")
      }
      case Mod(_) => eval(x._1, map)match {
        case Right(_) => evalMathP2(x._2, map)
        case Left(_) => Left("Oops, Somehow somewhere something went wrong")
      }
      case a: Identifier => map.get(a) match {
        case Some(k) => k match {
          case NumberType => evalMathP2(x._2, map)
          case _ => Left("Wrong Type")
        }
        case _ => Left("Variavel nao declarada")
      }
      case NumberConst(_) => evalMathP2(x._2, map)
      case _ => Left("Tipo Incompativel Encontrado")
    }
  }

  def evalAttribution(x: (Identifier, Token), map: Map[Identifier, `Type`]) = {

    val a = map.get(x._1)

    val b = a.isDefined

    if(b){
      val typeLeft = map(x._1)
      val c = x._2 match {
        case x: Identifier => map.get(x) match{
          case Some(k) => {
            if(k.equals(typeLeft)) Right(Success)
            else Left("Variaveis incompativeis")
          }
          case _ => Left("Variavel nao declarada")
        }
        case NumberConst(_) => {
          typeLeft match{
            case NumberType => Right(Success)
            case _ => Left("Variavel com tipos incompativel")
          }
        }
        case StringConst(_) => {
          typeLeft match {
            case StringType => Right(Success)
            case _ => Left("Variavel com tipo incompativel")
          }
        }
        case Bool(_) => {
          typeLeft match {
            case BoolType => Right(Success)
            case _ => Left("Variavel com tipo incompativel")
          }
        }
        case Add(_) => eval(x._2, map)
        case Sub(_) => eval(x._2, map)
        case Div(_) => eval(x._2, map)
        case Mul(_) => eval(x._2, map)
        case Mod(_) => eval(x._2, map)
      }
      c
    }
    else Left("Variavel Nao Declarada")
  }


  def evalRelational(x: (Token, Token), map: Map[Identifier, `Type`]) = {

    val a = x._1 match {
      case Add(_) => eval(x._1, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case Sub(_) => eval(x._1, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case Mul(_) => eval(x._1, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case Div(_) => eval(x._1, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case Mod(_) => eval(x._1, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case NumberConst(_) => Right(Success)
      case x: Identifier => map.get(x) match {
        case Some(k) => k match{
          case NumberType => Right(Success)
          case _ => Left("Wrong Type")
        }
        case _ => Left("Var not declared")
      }
      case _ => Left("Wrong Type")
    }

    val b = x._2 match {
      case Add(_) => eval(x._2, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case Sub(_) => eval(x._2, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case Mul(_) => eval(x._2, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case Div(_) => eval(x._2, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case Mod(_) => eval(x._2, map) match {
        case Right(_) => Right(Success)
        case Left(_) => Left("Wrong Type")
      }
      case NumberConst(_) => Right(Success)
      case x: Identifier => map.get(x) match {
        case Some(k) => k match{
          case NumberType => Right(Success)
          case _ => Left("Wrong Type")
        }
        case _ => Left("Var not declared")
      }
      case _ => Left("Wrong Type")
    }

    if(b.equals(a))
      Right(Success)
    else if(b.isLeft) b
    else a

  }



  def evalWhile(x: (Token, Seq[Token]), map: Map[Identifier, `Type`]) = {
    def recur(x: Seq[Token]):  Either[String, Status] = {
      if(x.isEmpty) Right(Success)
      else
        eval(x.head, map) match {
          case Right(_) => recur(x.tail)
          case Left(k) => Left(k)
        }
    }

    x._1 match{
      case Bool(_) => Right(Success)
      case _ => eval(x._1, map) match {
          case Right(_) => recur(x._2)
          case Left(k) => Left(k)
        }
    }
  }

  def evalIf(x: (Token, Seq[Token], Option[Seq[Token]]), map: Map[Identifier, `Type`]) = {
    def recur(x: Seq[Token]):  Either[String, Status] = {
      if(x.isEmpty) Right(Success)
      else
        eval(x.head, map) match {
          case Right(_) => recur(x.tail)
          case Left(k) => Left(k)
        }
    }
    val c = x._1 match{
      case Bool(_) => Right(Success)
      case _ => eval(x._1, map) match {
        case Right(_) => recur(x._2)
        case Left(k) => Left(k)
      }
    }

    if(c.isRight){
      val d = x._3 match {
        case Some(k) => recur(k)
        case None => c
      }
      d
    }else c
  }

  def eval(head: Token, map: Map[Identifier, `Type`]): Either[String, Status] = {
    head match {
      case For(x) => evalFor(x, map)
      case While(x) => evalWhile(x, map)
      case If(x) => evalIf(x, map)
      case Switch(x) => evalSwitch(x, map)
      case Case(x) => evalCase(x, map)
      case Attribution(x) => evalAttribution(x, map)
      case Write(x) => evalWrite(x, map)
      case ForAttribution(x) => evalForAttribution(x, map)
      case Iteration(x) => evalIteration(x, map)
      case Greater(x) => evalRelational(x, map)
      case GreaterOrEquals(x) => evalRelational(x, map)
      case Less(x) => evalRelational(x, map)
      case LessOrEquals(x) => evalRelational(x, map)
      case Equals(x) => evalRelational(x, map)
      case NotEquals(x) => evalRelational(x, map)
      case Add(x)=> evalMath(x, map)
      case Sub(x)=> evalMath(x, map)
      case Mul(x)=> evalMath(x, map)
      case Div(x)=> evalMath(x, map)
      case Mod(x)=> evalMath(x, map)
      case Read(_) => Right(Success)
    }
  }

  def Analise(program: Program): (Either[String, Status], Map[Identifier, `Type`])  = {
    def buildSymbolTable(declarationZone: DeclarationZone):Map[Identifier, `Type`] = {
      val x : Map[Identifier,`Type`] = Map.empty
      def recur(tokenList:Seq[Token], symbolTable:Map[Identifier, `Type`]):Map[Identifier, `Type`] = {
        if(tokenList.isEmpty) symbolTable
        else tokenList.head match {
            case Variable(value) => value._2 match {
            case NumberConst(_) => recur(tokenList.tail, symbolTable + (value._1 -> NumberType))
            case StringConst(_) => recur(tokenList.tail, symbolTable + (value._1 -> StringType))
            case Bool(_) => recur(tokenList.tail, symbolTable + (value._1 -> BoolType))
            case x:Identifier => recur(tokenList.tail, symbolTable + (value._1 -> symbolTable(x)))
          }

        }
      }
      recur(declarationZone.value, x)
    }
    val x = buildSymbolTable(program.value._1)
    def analyze(codeZone: CodeZone, map: Map[Identifier, `Type`]): Either[String, Status] ={
      def recur(tokenList: Seq[Token], success: Either[String,Status]): Either[String, Status] ={
        if(tokenList.isEmpty) success
        else success match{
          case Left(x) => Left(x)
          case Right(_) => recur(tokenList.tail, eval(tokenList.head, map))
        }
      }
      recur(codeZone.value, Right(Success))
    }

    (analyze(program.value._2, x), x)
  }
}
