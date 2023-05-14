package src

import BF.*
import scala.compiletime.constValue


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

object Main {
  def main(args: Array[String]): Unit = print(constValue[output]) // print result!
}
