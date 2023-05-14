package src

import scala.annotation.experimental
import scala.compiletime.ops.*
import scala.compiletime.ops.int.*
import scala.compiletime.constValue
import scala.compiletime.byName
import scala.compiletime.ops.string.{Substring, + => concat, Length}
import scala.Tuple.*

@experimental
object BF {

  // array of chars, because scala3 cannot convert 42.type to "*".type
  type chars = "\u0000\u0001\u0002\u0003\u0004\u0005\u0006\u0007\u0008\u0009\u000a\u000b\u000c\u000d\u000e\u000f\u0010\u0011\u0012\u0013\u0014\u0015\u0016\u0017\u0018\u0019\u001a\u001b\u001c\u001d\u001e\u001f\u0020\u0021\u0022\u0023\u0024\u0025\u0026\u0027\u0028\u0029\u002a\u002b\u002c\u002d\u002e\u002f\u0030\u0031\u0032\u0033\u0034\u0035\u0036\u0037\u0038\u0039\u003a\u003b\u003c\u003d\u003e\u003f\u0040\u0041\u0042\u0043\u0044\u0045\u0046\u0047\u0048\u0049\u004a\u004b\u004c\u004d\u004e\u004f\u0050\u0051\u0052\u0053\u0054\u0055\u0056\u0057\u0058\u0059\u005a\u005b\u005c\u005d\u005e\u005f\u0060\u0061\u0062\u0063\u0064\u0065\u0066\u0067\u0068\u0069\u006a\u006b\u006c\u006d\u006e\u006f\u0070\u0071\u0072\u0073\u0074\u0075\u0076\u0077\u0078\u0079\u007a\u007b\u007c\u007d\u007e\u007f\u0080\u0081\u0082\u0083\u0084\u0085\u0086\u0087\u0088\u0089\u008a\u008b\u008c\u008d\u008e\u008f\u0090\u0091\u0092\u0093\u0094\u0095\u0096\u0097\u0098\u0099\u009a\u009b\u009c\u009d\u009e\u009f\u00a0\u00a1\u00a2\u00a3\u00a4\u00a5\u00a6\u00a7\u00a8\u00a9\u00aa\u00ab\u00ac\u00ad\u00ae\u00af\u00b0\u00b1\u00b2\u00b3\u00b4\u00b5\u00b6\u00b7\u00b8\u00b9\u00ba\u00bb\u00bc\u00bd\u00be\u00bf\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c6\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf\u00d0\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d7\u00d8\u00d9\u00da\u00db\u00dc\u00dd\u00de\u00df\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e6\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f0\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f7\u00f8\u00f9\u00fa\u00fb\u00fc\u00fd\u00fe\u00ff"

  type Memory = Tuple // Tuple with Int's
  type State = Tuple // (codePointer: Int, memoryPointer: Int, memory: Tuple of ints, input: String, output: String)
  type Code = String
  type Error[msg <: String] = Nothing

  type MakeMemory[size <: Int, acc <: Tuple] = size match {
    case 0 => acc
    case _ => MakeMemory[size - 1, 0 *: acc]
  }

  type GetChar[string <: String, index <: Int] = Substring[string, index, index + 1]
  type ByteToChar[byte <: Int] = GetChar[chars, byte]
  type CharToByte[char <: String, byte <: Int] =
    byte match {
      case 256 => 0
      case _ => GetChar[chars, byte] match {
        case char => byte
        case _ => CharToByte[char, byte + 1]
      }
    }

  type MemorySet[memory <: Memory, memoryPointer <: Int, value <: Int] = Split[memory, memoryPointer] match {
    case (left, _ *: right) => Concat[left, value *: right]
  }
  type MemoryInc[memory <: Memory, memoryPointer <: Int] = Split[memory, memoryPointer] match {
    case (left, value *: right) => Concat[left, S[value] *: right]
  }
  type MemoryDec[memory <: Memory, memoryPointer <: Int] = Split[memory, memoryPointer] match {
    case (left, S[value] *: right) => Concat[left, value *: right]
  }

  type FindClosingBracket[code <: Code, codePointer <: Int, nesting <: Int] =
    GetChar[code, codePointer] match {
      case "]" => nesting match {
        case 0 => codePointer
        case _ => FindClosingBracket[code, codePointer + 1, nesting - 1]
      }
      case "[" => FindClosingBracket[code, codePointer + 1, nesting + 1]
      case _ => FindClosingBracket[code, codePointer + 1, nesting]
    }

  type FindOpeningBracket[code <: Code, pointer <: Int, nesting <: Int] =
    GetChar[code, pointer] match {
      case "]" => FindOpeningBracket[code, pointer - 1, nesting - 1]
      case "[" => nesting match {
        case 0 => pointer
        case _ => FindOpeningBracket[code, pointer - 1, nesting + 1]
      }
      case _ => FindOpeningBracket[code, pointer - 1, nesting]
    }

  type CodeIteration[code <: Code, state <: State] =
    state match {
      case (codePointer, memoryPointer, memory, in, out) => GetChar[code, codePointer] match {
        case ">" => (codePointer + 1, memoryPointer + 1, memory, in, out)
        case "<" => (codePointer + 1, memoryPointer - 1, memory, in, out)
        case "+" => (codePointer + 1, memoryPointer, MemoryInc[memory, memoryPointer], in, out)
        case "-" => (codePointer + 1, memoryPointer, MemoryDec[memory, memoryPointer], in, out)
        case "[" => Elem[memory, memoryPointer] match {
          case 0 => (FindClosingBracket[code, codePointer + 1, 0], memoryPointer, memory, in, out)
          case _ => (codePointer + 1, memoryPointer, memory, in, out)
        }
        case "." => (codePointer + 1, memoryPointer, memory, in, out concat ByteToChar[Elem[memory, memoryPointer]])
        case "," => (codePointer + 1, memoryPointer, MemorySet[memory, memoryPointer, CharToByte[GetChar[in, 0], 0]], Substring[in, 1, Length[in]], out)
        case "]" => Elem[memory, memoryPointer] match {
          case 0 => (codePointer + 1, memoryPointer, memory, in, out)
          case _ => (FindOpeningBracket[code, codePointer - 1, 0], memoryPointer, memory, in, out)
        }
        case _ => (codePointer + 1, memoryPointer, memory, in, out) // skip unknown chars in code
      }
    }

  type CodeLoop[code <: Code, state <: State] = state match {
    case (codePointer, memoryPointer, memory, in, out) => codePointer >= 0 match {
      case false => Error["Negative code pointer"]
      case true => codePointer < Length[code] match {
        case false =>
        state match {
          case (codePointer, memoryPointer, memory, in, out) => out
        }
        case true => CodeLoop[code, CodeIteration[code, (codePointer, memoryPointer, memory, in, out)]]
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    import BF.*

    // Write your brainfuck code here
    // Unknown characters are ignored
    type code = ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+."

    // String of chars for ',' operation
    type input = ""

    // Amount of memory cells. Each cell can hold signed integer
    type memorySize = 32

    // Starting state
    type emptyState = (0, 0, MakeMemory[memorySize, EmptyTuple], input, "")

    // Run brainfuck...
    type output = CodeLoop[code, emptyState]

    // print result!
    print(constValue[output])
  }
}
