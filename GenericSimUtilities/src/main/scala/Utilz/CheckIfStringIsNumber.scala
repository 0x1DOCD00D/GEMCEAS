package Utilz

object CheckIfStringIsNumber:
  import scala.util.Try
    def isShort(p: String): Boolean = Try(p.toLong).isSuccess
    def isInt(p: String): Boolean = Try(p.toInt).isSuccess
    def isLong(p: String): Boolean = Try(p.toLong).isSuccess
    def isDouble(p: String): Boolean = Try(p.toDouble).isSuccess
    def isFloat(p: String): Boolean = Try(p.toFloat).isSuccess
    extension (x: String)
      def IsNumber: Boolean = isShort(x) || isInt(x) || isLong(x) || isDouble(x) || isFloat(x)
