package src

import scala.annotation.experimental
import scala.compiletime.ops.*
import scala.compiletime.ops.int.*
import scala.compiletime.constValue
import scala.compiletime.byName
import scala.compiletime.ops.string.{Substring, + => concat, Length}
import scala.Tuple.*
//import scala.compiletime.ops.string.{*, + => str_+}
//import scala.compiletime.ops.string.*
//class Kek[A,B]
//type ![A, G]
//type !! = [A, B] =>> Int

type Iteration

@experimental
object BF {

  type chars = "\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000a\u000b\u000c\u000d\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f\u0020\u0021\u0022\u0023\u0024\u0025\u0026\u0027\u0028\u0029\u002a\u002b\u002c\u002d\u002e\u002f\u0030\u0031\u0032\u0033\u0034\u0035\u0036\u0037\u0038\u0039\u003a\u003b\u003c\u003d\u003e\u003f\u0040\u0041\u0042\u0043\u0044\u0045\u0046\u0047\u0048\u0049\u004a\u004b\u004c\u004d\u004e\u004f\u0050\u0051\u0052\u0053\u0054\u0055\u0056\u0057\u0058\u0059\u005a\u005b\u005c\u005d\u005e\u005f\u0060\u0061\u0062\u0063\u0064\u0065\u0066\u0067\u0068\u0069\u006a\u006b\u006c\u006d\u006e\u006f\u0070\u0071\u0072\u0073\u0074\u0075\u0076\u0077\u0078\u0079\u007a\u007b\u007c\u007d\u007e\u007f\u0080\u0081\u0082\u0083\u0084\u0085\u0086\u0087\u0088\u0089\u008a\u008b\u008c\u008d\u008e\u008f\u0090\u0091\u0092\u0093\u0094\u0095\u0096\u0097\u0098\u0099\u009a\u009b\u009c\u009d\u009e\u009f\u00a0\u00a1\u00a2\u00a3\u00a4\u00a5\u00a6\u00a7\u00a8\u00a9\u00aa\u00ab\u00ac\u00ad\u00ae\u00af\u00b0\u00b1\u00b2\u00b3\u00b4\u00b5\u00b6\u00b7\u00b8\u00b9\u00ba\u00bb\u00bc\u00bd\u00be\u00bf\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c6\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf\u00d0\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d7\u00d8\u00d9\u00da\u00db\u00dc\u00dd\u00de\u00df\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e6\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f0\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f7\u00f8\u00f9\u00fa\u00fb\u00fc\u00fd\u00fe\u00ff"
  type GetChar[Str <: String, Index <: Int] = Substring[Str, Index, Index + 1]
  type ByteToChar[byte <: Int] = GetChar[chars, byte]
  type CharToByte[char <: String, byte <: Int] =
    byte match {
      case 256 => 0
      case _ => GetChar[chars, byte] match {
        case char => byte
        case _ => CharToByte[char, byte + 1]
      }
    }


  type Memory = Tuple // Tuple with Int's
  type MakeMemory[MemorySize <: Int, Acc <: Tuple] = MemorySize match {
    case 0 => Acc
    case _ => MakeMemory[MemorySize - 1, 0 *: Acc]
  }

  type MemorySet[memory <: Memory, memoryPointer <: Int, value <: Int] = MemorySetIter[memory, memoryPointer, value, 0]
  type MemorySetIter[memory <: Memory, memoryPointer <: Int, value <: Int, index <: Int] = memory match {
    case EmptyTuple => EmptyTuple
    case x *: xs =>
    index match {
      case memoryPointer => value *: MemorySetIter[xs, memoryPointer, value, index + 1]
      case _ => x *: MemorySetIter[xs, memoryPointer, value, index + 1]
    }
  }

  type For[index <: Int, cap <: Int, func[_ <: Int], acc <: Tuple] = index - cap match {
    case 0 => acc
    case _ => For[index + 1, cap, func, func[index] *: acc]
  }


  type Err[msg <: String] = Nothing

  type State = Tuple // (pointer: Int)
  type Code = String

  type FindClosingBracket[code <: Code, pointer <: Int, nested <: Int] =
    GetChar[code, pointer] match {
      case "]" => nested match {
        case 0 => pointer
        case _ => FindClosingBracket[code, pointer + 1, nested - 1]
      }
      case "[" => FindClosingBracket[code, pointer + 1, nested + 1]
      case _ => FindClosingBracket[code, pointer + 1, nested]
    }

  type FindOpenBracket[code <: Code, pointer <: Int, nested <: Int] =
    GetChar[code, pointer] match {
      case "]" => FindOpenBracket[code, pointer - 1, nested - 1]
      case "[" => nested match {
        case 0 => pointer
        case _ => FindOpenBracket[code, pointer - 1, nested + 1]
      }
      case _ => FindOpenBracket[code, pointer - 1, nested]
    }

  type CodeIter[code <: Code, state <: State] =
    state match {
      case (codePointer, memoryPointer, memory, in, out) => GetChar[code, codePointer] match {
        case ">" => (codePointer + 1, memoryPointer + 1, memory, in, out)
        case "<" => (codePointer + 1, memoryPointer - 1, memory, in, out)
        case "+" => (codePointer + 1, memoryPointer, MemorySet[memory, memoryPointer, Elem[memory, memoryPointer] + 1], in, out)
        case "-" => (codePointer + 1, memoryPointer, MemorySet[memory, memoryPointer, Elem[memory, memoryPointer] - 1], in, out)
        case "[" => Elem[memory, memoryPointer] match {
          case 0 => (FindClosingBracket[code, codePointer + 1, 0], memoryPointer, memory, in, out)
          case _ => (codePointer + 1, memoryPointer, memory, in, out)
        }
        case "." => (codePointer + 1, memoryPointer, memory, in, out concat ByteToChar[Elem[memory, memoryPointer]]) //
        case "," => (codePointer + 1, memoryPointer, MemorySet[memory, memoryPointer, CharToByte[GetChar[in, 0], 0]], Substring[in, 1, Length[in]], out)
        case "]" => Elem[memory, memoryPointer] match {
          case 0 => (codePointer + 1, memoryPointer, memory, in, out)
          case _ => (FindOpenBracket[code, codePointer - 1, 0], memoryPointer, memory, in, out)
        }
        case _ => Err["unknown char"] //(codePointer + 1, memoryPointer, memory, in, out)
      }
    }

  type RunCode[code <: Code, state <: State] = state match {
    case (codePointer, memoryPointer, memory, in, out) => codePointer >= 0 match {
      case false => Err["negative code pointer"] // bug in interpreter?
      case true => codePointer < Length[code] match {
        case false =>  Err["ERE char" concat state]
//        state match {
//          case (codePointer, memoryPointer, memory, in, out) => out
//        }
        case true => RunCode[code, CodeIter[code, (codePointer, memoryPointer, memory, in, out)]]
      }
    }
  }
}

object Main {

  //  def kek(a: )
  def main(args: Array[String]): Unit = {
    import BF.*
    //    val k: StrToTupleIter["0123456789", Tuple1[Int]]  = Tuple1(1)
    type Identity[T] = T
    //    print(implicitly[ValueOf[For[0, 256, Identity, EmptyTuple]]].value)
    //    print(implicitly[ValueOf[1 *: EmptyTuple]].value)

    //        print(implicitly[ValueOf[RunCode["><>", (0, 0, MakeMemory[16, EmptyTuple])]]].value)

    //    type K="1"+"b"?
    //    print(constValue["K" concat "2"])
    //print("\u0072")
    //        print(implicitly[ValueOf[FindOpenBracket["[[]12",2,0]]].value)
    //    print(implicitly[ValueOf[MemorySet[(1, 2, 3), 14, 42, 0]]])


    //    type kek = RunCode["+++++++++++++++++++++++++++++++++++++++++++++\n +++++++++++++++++++++++++++.+++++++++++++++++\n ++++++++++++.+++++++..+++.-------------------\n ---------------------------------------------\n ---------------.+++++++++++++++++++++++++++++\n ++++++++++++++++++++++++++.++++++++++++++++++\n ++++++.+++.------.--------.------------------\n ---------------------------------------------\n ----.-----------------------.", (0, 0, MakeMemory[16, EmptyTuple], "", "")]
    print(implicitly[ValueOf[RunCode[">>>>>>>>>>>>>", (0, 0, MakeMemory[16, EmptyTuple], "", "")]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
//    print(implicitly[ValueOf[MemorySet[(0,0,0),0,9]]].value)
  }
}
