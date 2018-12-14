import sexp._

sealed abstract class Exp
//case class Add(lhs: Exp, rhs: Exp) extends Exp
//case class Multiply(lhs: Exp, rhs: Exp) extends Exp
//case class Equals(exp1 : Exp, exp2 : Exp) extends Exp
//case class Cons(exp1 : Exp, exp2: Exp) extends Exp
//case class Car(args : Exp) extends Exp
//case class Cdr(args : Exp) extends Exp
//case class Pair(args : Exp) extends Exp
//case class Null(args : Exp) extends Exp
//case class NullHuh(args : Exp) extends Exp
//case class LessThan(exp1 : Exp, exp2: Exp) extends Exp
case object Null extends Exp
case class Literal(v: SExp) extends Exp
case class Bool(v: Boolean) extends Exp
case class Call(lhs : Exp, rhs : List[Exp]) extends Exp
case class Ref(v : String) extends Exp
case class If(condition : Exp, ifFtrue :Exp, ifFalse : Exp) extends Exp
case class Def(name : String, args : List[String], body : Exp)
case class Program(defs : List[Def], exp : Exp)
case class Quote(in : SExp) extends Exp
case class Lambda(id: List[String], body: Exp) extends Exp
case class Closure(args: List[String], body : Exp, env: Env) extends SExp
case class Primitive(name : String) extends SExp
class Box[A](var contents: Option[A] = None)


def parseDefine(name : String, args : SExp, body : SExp,  thelist : List[String]) : Def =
  args match {
    case SNil => Def(name, thelist.reverse, parseExp(body))
    case SCons(SSymbol(id), rest) => parseDefine(name, rest, body, id :: thelist)
  }

def parseProgram(p : SExp) : Program = {
  helperParse(p, List())
}

def helperParse(p : SExp, accumulator : List[Def]) : Program = {
  p match {
    case SList(first) => Program(accumulator.reverse, parseExp(first))
    case SCons(SList(SSymbol("define"),SCons(SSymbol(name), args), body), rest) => helperParse(rest, parseDefine(name, args, body, List()) :: accumulator)
    case _ => throw new RuntimeException("hello")
  }
}

def parseLambda(id: SExp, body : SExp, accumulator: List[String]) : Lambda = id match {
    case SNil => Lambda(accumulator.reverse, parseExp(body))
    case SCons(SSymbol(id), rest) => parseLambda(rest, body, id :: accumulator)
  }

def parseCall(arg: SExp, id: Exp, accumulator : List[Exp]) : Call = arg match {
    case SNil => Call(id, accumulator.reverse)
    case SCons(first, rest) => parseCall(rest, id, parseExp(first) :: accumulator)
  }

def parseExp(e: SExp) : Exp = {
  e match {

   /*
    case SList(SSymbol("+"), l, r) =>
      Add(parseExp(l), parseExp(r))
    case SList(SSymbol("*"), l, r) =>
      Multiply(parseExp(l), parseExp(r))
    case SList(SSymbol("null?"), args) =>
      NullHuh(parseExp(args))
    case SList(SSymbol("equal?"),exp1, exp2) =>
      Equals(parseExp(exp1), parseExp(exp2))
    case SList(SSymbol("<"), exp1, exp2) =>
     LessThan(parseExp(exp1), parseExp(exp2))
    case SList(SSymbol("cons"), exp1, exp2) =>
      Cons(parseExp(exp1), parseExp(exp2))
    case SList(SSymbol("car"), args) =>
      Car(parseExp(args))
    case SList(SSymbol("cdr"), args) =>
      Cdr(parseExp(args))
    case SList(SSymbol("pair?"), args) =>
      Pair(parseExp(args))
    case SCons(SSymbol(id), rest) =>
      def expressionHelper (stuff : SExp, accumulator : List[Exp]) : List[Exp] =
        stuff match {
          case SNil => accumulator
          case SCons(first,rest) => expressionHelper(rest, parseExp(first) :: accumulator)
        }
      Call(id,expressionHelper(rest,List()))
    */
    case SList(SSymbol("quote"), in) => Quote(in)
    case SList(SSymbol("if"), condition, ifTrue, ifFalse) =>
      If(parseExp(condition), parseExp(ifTrue), parseExp(ifFalse))
    case STrue() => Bool(true)
    case SFalse() => Bool(false)
    case SList(SSymbol("lambda"),SList(SSymbol(id)),body) =>
      Lambda(List(id), parseExp(body))
    case SInt(v) => Literal(SInt(v))
    case SList(SSymbol("let"), SList(SList(SSymbol(id), rhs)), body) =>
      Call(Lambda(List(id), parseExp(body)), List(parseExp(rhs)))
    case SCons(e1, e2) =>
      parseCall(e2, parseExp(e1), List())
    case SSymbol(id) => Ref(id)
    case _ => throw new IllegalArgumentException(
      "not a valid expression")

  }
}

type Env = Map[String, Box[SExp]]

//type Env2 = Map[String, Def]
/*
//a helper that maps function names to their expressions
def mapping(arg : List[String], input : List[SExp], env : Env) : Env = {
  (arg,input) match {
    case (Nil,Nil) => env
    case (Nil, _) => throw new RuntimeException("blarg2")
    case (_ ,Nil) => throw new RuntimeException("blarg3")
    case (af :: ar, vf :: vr) =>
      val newEnv = env + (af -> vf)
      mapping(ar, vr, newEnv)
  }
}
*/

def interpExp(e: Exp, env : Env) : SExp = {
  e match {
    case Literal(v) => v
    case Bool(true) => STrue()
    case Bool(false) => SFalse()
    case Ref(id) => env.get(id) match {
      case None => throw new RuntimeException("gitgud")
      case Some(v) => v.contents match {
        case None => throw new RuntimeException("gitgud")
        case Some(v) => v
      }
    }
    case Null => SNil
    case If(condition, ifFtrue, ifFalse) => {
      if (interpExp(condition, env) != SFalse()) {
        interpExp(ifFtrue, env)
      } else {
        interpExp(ifFalse, env)
      }
    }

    case Quote(arguments) => arguments
    case Lambda(id, body) => Closure(id, body, env)
    case Call(name, arg) => name match {
      case Ref(name) => env.get(name) match {
        case None => throw new RuntimeException("Undef " + name)
        case Some(v) => v.contents match {
          case None => throw new RuntimeException("Undef "+ v)
          case Some(Closure(args, body, ending)) => interpExp(body, toEnv(args zip arg, ending, env))
          case Some(Primitive(operation)) => operation match {
            case "+" => arg.map(e => interpExp(e, env)) match {
              case List(SInt(arg1), SInt(arg2)) => SInt(arg1 + arg2)
              case _ => throw new UnsupportedOperationException("gitgud")
            } case "-" => arg.map(e => interpExp(e, env)) match {
              case List(SInt(arg1), SInt(arg2)) => SInt(arg1 - arg2)
              case _ => throw new UnsupportedOperationException("gitgud")
            } case "*" => arg.map(e => interpExp(e, env)) match {
              case List(SInt(arg1), SInt(arg2)) => SInt(arg1 * arg2)
              case _ => throw new UnsupportedOperationException("gitgud")
            } case "null?" => arg.map(e => interpExp(e, env)) match {
              case List(SNil) => STrue()
              case _ => SFalse()
            } case "pair?" => arg.map(e => interpExp(e, env)) match {
              case List(SCons(head, tail)) => STrue()
              case _ => SFalse()
            } case "cons" => arg.map(e => interpExp(e, env)) match {
              case List(head: SExp, tail: SExp) => SCons(head, tail)
              case _ => throw new UnsupportedOperationException("gitgud")
            } case "car" => arg.map(e => interpExp(e, env)) match {
              case List(SNil) => SNil
              case List(SCons(head, tail)) => head
              case _ => throw new UnsupportedOperationException("gitgud")
            } case "cdr" => arg.map(e => interpExp(e, env)) match {
              case List(SNil) => SNil
              case List(SCons(head, tail)) => tail
              case _ => throw new UnsupportedOperationException("gitgud")
            } case "equal?" => arg.map(e => interpExp(e, env)) match {
              case List(lhs: Closure, rhs: Closure) => throw new RuntimeException("gitgud")
              case List(lhs: Primitive, rhs: Primitive) => throw new RuntimeException("gitgud")
              case List(lhs: SExp, rhs: SExp) => if (lhs == rhs) STrue() else SFalse()
              case _ => throw new RuntimeException("gitgud")
            }
          } case Some(v) => throw new RuntimeException("gitgud")
        }
      } case Lambda(args, body) => interpExp(Lambda(args, body), env) match {
        case Closure(args, body, ending) => interpExp(body, toEnv(args zip arg, ending, env))
      } case exp: Exp => interpExp(exp, env) match {
        case Closure(args, body, ending) => interpExp(body, toEnv(args zip arg, ending, env))
        case _ => throw new RuntimeException("gitgud")
      }
    }
    /*
    case Add(l, r) => {
      val v1 = interpExp(l, env, other)
      val v2 = interpExp(r, env, other)
      (v1, v2) match {
        case (SInt(v1), SInt(v2)) => SInt(v1 + v2)
        case _ => throw new RuntimeException("Herro (._.)")
      }
    }
    case Multiply(l, r) => {
      val v1 = interpExp(l, env, other)
      val v2 = interpExp(r, env, other)
      (v1, v2) match {
        case (SInt(v1), SInt(v2)) => SInt(v1 * v2)
        case _ => throw new RuntimeException("Herro")
      }
    }
    case Let(id, rhs, body) => {
      val rhsVal = interpExp(rhs, env, other)
      val newEnv = env + (id -> rhsVal)
      interpExp(body, newEnv, other)
    }
    case Equals(exp1, exp2) =>
      val v1 = interpExp(exp1, env, other)
      val v2 = interpExp(exp2, env, other)
      if (v1 == v2) {
        STrue()
      } else {
        SFalse()
      }

    case LessThan(exp1, exp2) =>
      val SInt(v1) = interpExp(exp1, env, other)
      val SInt(v2) = interpExp(exp2, env, other)
      if (v1 < v2) {
        STrue()
      } else {
        SFalse()
      }
    case Cons(exp1, exp2) => {
      val exp3 = interpExp(exp1, env, other)
      val exp4 = interpExp(exp2, env, other)
      SCons(exp3, exp4)
    }

    case Car(args) => interpExp(args, env, other) match {
      case SCons(first, rest) => first
      case _ => throw new RuntimeException("herroo")
    }
    case Cdr(args) => interpExp(args, env, other) match {
      case SCons(first, rest) => first
      case _ => throw new RuntimeException("herroo")
    }
    case Pair(args) => interpExp(args, env, other) match {
      case SCons(first, rest) => STrue()
      case _ => SFalse()
    }
    case Null(args) => interpExp(args, env, other) match {
      case SCons(first, rest) => SFalse()
      case _ => STrue()
    }
    case NullHuh(args) => interpExp(args, env, other) match {
      case SNil => STrue()
      case _ => SFalse()
    }
    */
  }
}

def interpProgram(p: Program, e: Env) : SExp = {
  var env = Map(
    "+" -> new Box[SExp](Some(Primitive("+"))),
    "-" -> new Box[SExp](Some(Primitive("-"))),
    "*" -> new Box[SExp](Some(Primitive("*"))),
    "cons" -> new Box[SExp](Some(Primitive("cons"))),
    "car" -> new Box[SExp](Some(Primitive("car"))),
    "cdr" -> new Box[SExp](Some(Primitive("cdr"))),
    "equal?" -> new Box[SExp](Some(Primitive("equal?"))),
    "pair?" -> new Box[SExp](Some(Primitive("pair?"))),
    "null?" -> new Box[SExp](Some(Primitive("null?"))),
    "null" -> new Box[SExp](Some(SNil)),
    "quote" -> new Box[SExp](Some(Primitive("quote")))
  ) ++ e
  progRun(p, defFunc(p, initFunc(p, env)))
}

def DefMap(ldef : List[Def], ret : Map[String, Def]) : Map[String, Def] =  ldef match {
  case Nil => ret
  case first::rest => Map()
    val Def(name, argument, body) = first
    DefMap(rest, ret + (name -> first))
}
def evalProgram(p : String) : SExp =
  interpProgram(parseProgram(parseSExp(p)), Map())

def initFunc(p: Program, env: Env) : Env = p.defs match {
    case Nil => env
    case first :: rest => initFunc(Program(rest, p.exp), env + (first.name -> new Box()))
  }

def defFunc(p: Program, env: Env) : Env = {
  p.defs.foreach { item => env.get(item.name) match {
      case None => throw new RuntimeException("a")
      case Some(v) => v.contents = Some(Closure(item.args, item.body, env))
    }
  }
  env
}

def progRun(p: Program, env: Env) : SExp = p.defs match {
    case Nil => interpExp(p.exp, env)
    case first :: rest => progRun(Program(rest, p.exp),
        env + (first.name -> new Box(Some(Closure(first.args, first.body, env))))
      )
  }

def toEnv(argument : List[(String, Exp)], accumulator: Env, env: Env) : Env = argument  match {
    case Nil => accumulator
    case first :: rest =>
      toEnv(rest, accumulator + (first._1 -> new Box(Some(interpExp(first._2, env)))), env)
  }

def evalExp(e : String) : SExp=
  interpExp(parseExp(parseSExp(e)), Map())

// vim: set ts=2 sw=2 et:

assert(
  evalProgram(
    """
  (
    (* (+ 1 2) 4)
  )
  """
  )
    ==
    SInt(12)
)

// Let

assert(
  evalProgram(
    """
  (
    (let ((x (+ 1 2)))
      (* x 4))
  )
  """
  )
    ==
    SInt(12)
)

assert(
  evalProgram(
    """
  (
    (let ((x (let ((y 1))
               (+ y 2))))
      (* x 4))
  )
  """
  )
    ==
    SInt(12)
)

assert(
  evalProgram(
    """
  (
    (let ((x 2))
      (let ((y 4))
        (let ((x 3))
          (* x y))))
  )
  """
  )
    ==
    SInt(12)
)

// Lists

assert(
  evalProgram(
    """
  (null)
  """
  )
    ==
    SNil
)

assert(
  evalProgram(
    """
  (
    (let ((x null))
      (null? x))
  )
  """
  )
    ==
    STrue()
)

assert(
  evalProgram(
    """
  (
    (let ((x null))
      (pair? x))
  )
  """
  )
    ==
    SFalse()
)

assert(
  evalProgram(
    """
  (
    (let ((l (cons 1 2)))
      (car l))
  )
  """
  )
    ==
    SInt(1)
)

assert(
  evalProgram(
    """
  (
    (let ((l (cons 1 2)))
      (cdr l))
  )
  """
  )
    ==
    SInt(2)
)

assert(
  evalProgram(
    """
  (
    (let ((l (cons 1 2)))
      (pair? l))
  )
  """
  )
    ==
    STrue()
)

assert(
  evalProgram(
    """
  (
    (let ((l (cons 1 2)))
      (null? l))
  )
  """
  )
    ==
    SFalse()
)

assert(
  evalProgram(
    """
  (
    (null? 5)
  )
  """
  )
    ==
    SFalse()
)

assert(
  evalProgram(
    """
  (
    (pair? 5)
  )
  """
  )
    ==
    SFalse()
)

// Conditionals

assert(
  evalProgram(
    """
  (
    (let ((c #f))
      (if c 5 6))
  )
  """
  )
    ==
    SInt(6)
)

assert(
  evalProgram(
    """
  (
    (let ((c (cons 1 2)))
      (if c 5 6))
  )
  """
  )
    ==
    SInt(5)
)

assert(
  evalProgram(
    """
  (
    (let ((c #t))
      (if c 5 6))
  )
  """
  )
    ==
    SInt(5)
)

assert(
  evalProgram(
    """
  (
    (let ((v1 5))
      (equal? v1 5))
  )
  """
  )
    ==
    STrue()
)

assert(
  evalProgram(
    """
  (
    (let ((v1 5))
      (equal? v1 6))
  )
  """
  )
    ==
    SFalse()
)

assert(
  evalProgram(
    """
  (
    (let ((v1 #f))
      (equal? v1 #f))
  )
  """
  )
    ==
    STrue()
)


// Define

assert(
  evalProgram(
    """
  (
    (define (f)
      5)
    (f)
  )
  """
  )
    ==
    SInt(5)
)

assert(
  evalProgram(
    """
  (
    (define (f arg)
      (+ 1 arg))
    (f 5)
  )
  """
  )
    ==
    SInt(6)
)

assert(
  evalProgram(
    """
  (
    (define (g) 1)
    (define (f arg)
      (+ (g) arg))
    (f 5)
  )
  """
  )
    ==
    SInt(6)
)

// Checks that reference to variable by dynamic scope is an error;
// you should implement lexical scope.
assert(
  try {
    evalProgram(
      """
      (
        (define (g) x)
        (define (f x)
          (g))
        (f 5)
      )
    """)
    false
  } catch {
    case e: Exception => true
  }
)

// Lambda

assert(evalProgram(
  """
  ((let ((double (lambda (n) (* n 2))))
    (double 5)))
  """) == SInt(10))

assert(evalProgram(
  """
  ((let ((two 2))
    (let ((double (lambda (n) (* n two))))
      (double 5))))
  """) == SInt(10))

assert(evalProgram(
  """
  ((((lambda (x) (lambda (y) x))
    5)
   6))
  """)
  == SInt(5))

assert(evalProgram(
  """
  ((let ((twice (lambda (f) (lambda (arg) (f (f arg))))))
    ((twice (lambda (x) (* x 2))) 5)))
  """)
  == SInt(20))

// Higher-order primitives

assert(evalProgram(
  """
  ((let ((apply (lambda (f) (f 3 4))))
     (cons (apply +)
           (apply cons))))
  """
) == SCons(SInt(7), SCons(SInt(3), SInt(4))))

assert(evalProgram(
  """
  ((define (foldLeft f l acc)
   (if (null? l)
     acc
     (foldLeft f (cdr l) (f (car l) acc))))
 (foldLeft + (quote (1 2 3)) 0))
 """)
  == SInt(6))

// Real programs

assert(
  evalProgram("""
  ((define (append l s)
   (if (null? l)
     s
     (cons (car l) (append (cdr l) s))))
   (append (quote (1 2 3)) (quote (4 5 6))))

  """)
    ==
    SList(SInt(1), SInt(2), SInt(3), SInt(4), SInt(5), SInt(6))
)

assert(
  evalProgram(
    """
    (
    (define (even? n)
      (if (equal? n 0)
        #t
        (odd? (- n 1))))
    (define (odd? n)
      (if (equal? n 0)
        #f
        (even? (- n 1))))
    (even? 10)
    )
    """)
    ==
    STrue()
)

// If you're curious what this is, dig in here!
// http://matt.might.net/articles/compiling-up-to-lambda-calculus/
assert(evalProgram(
  """
  ((define (succ n) (+ n 1))
   (define (natify church-numeral)
     ((church-numeral succ) 0))
   (natify ((lambda (f) (f (lambda (f) (lambda (z) (f (f (f (f (f z)))))))))
      (((lambda (y) (lambda (F) (F (lambda (x) (((y y) F) x)))))
        (lambda (y) (lambda (F) (F (lambda (x) (((y y) F) x))))))
      (lambda (f)
        (lambda (n)
          ((((((lambda (n)
            ((n (lambda (_) (lambda (t) (lambda (f) (f (lambda (void) void))))))
              (lambda (t) (lambda (f) (t (lambda (void) void))))))
            (((lambda (n)
              (lambda (m)
                ((m
                  (lambda (n)
                    (lambda (f)
                      (lambda (z)
                        (((n (lambda (g) (lambda (h) (h (g f)))))
                          (lambda (u) z))
                        (lambda (u) u))))))
                      n)))
                    n)
                  (lambda (f) (lambda (z) z))))
                (lambda (_)
                  ((lambda (n)
                    ((n (lambda (_) (lambda (t) (lambda (f) (f (lambda (void) void))))))
                      (lambda (t) (lambda (f) (t (lambda (void) void))))))
                    (((lambda (n)
                      (lambda (m)
                        ((m
                          (lambda (n)
                            (lambda (f)
                              (lambda (z)
                                (((n (lambda (g) (lambda (h) (h (g f)))))
                                  (lambda (u) z))
                                (lambda (u) u))))))
                              n)))
                            (lambda (f) (lambda (z) z)))
                          n))))
                        (lambda (_) (lambda (t) (lambda (f) (f (lambda (void) void))))))
                      (lambda (_) (lambda (f) (lambda (z) (f z)))))
                    (lambda (_)
                      (((lambda (n) (lambda (m) (lambda (f) (lambda (z) ((m (n f)) z))))) n)
                        (f
                          (((lambda (n)
                            (lambda (m)
                              ((m
                                (lambda (n)
                                  (lambda (f)
                                    (lambda (z)
                                      (((n (lambda (g) (lambda (h) (h (g f)))))
                                        (lambda (u) z))
                                      (lambda (u) u))))))
                                    n)))
                                  n)
                                (lambda (f) (lambda (z) (f z))))))))))))))
                              """
)
  == SInt(120))

// vim: set ts=2 sw=2 et: