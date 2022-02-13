package scalaCourse

object scalaObject {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
        var next: ListNode = _next
        var x: Int = _x
    }

  def main(args :Array[String]) {
  /* var array1 :Array[Int] = Array(1,2)
    var array2 :Array[Int] = Array(3,4)
    println("find median "+findMedianSortedArrays(array1,array2))*/
    var a = new ListNode(3,new ListNode(4,new ListNode(2,null)))
    var b = new ListNode(5,new ListNode(6,new ListNode(4,null)))
  println("sum of two lists == "+addTwoNumbers(a,b).x)
  println("longest palindrome === "+longestPalindrome("abcbaxyzyxa"))

  }
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]) : Double = {
    if( nums1.length > nums2.length ) return findMedianSortedArrays(nums2,nums1)
    val x = nums1.length
    val y = nums2.length
    var low = 0
    var high = x
    var result : Double = 0.0

    while (low <= high)
    {
      val partitionX = (low+high)/2
      val partitionY = ((x+y+1)/2 - partitionX )
      println("partition X "+partitionX)
      println("partition Y "+partitionY)
      val maxLeftX = if (partitionX == 0) Integer.MIN_VALUE else nums1(partitionX -1 )
      val minRightX = if (partitionX == x) Integer.MAX_VALUE else nums1(partitionX)
      val maxLeftY = if (partitionY == 0) Integer.MIN_VALUE else nums2(partitionY -1 )
      val minRightY = if (partitionY == y) Integer.MAX_VALUE else nums2(partitionY)
      println("If condition maxLeftX "+maxLeftX+" minRightY "+minRightY+" maxLeftY "+maxLeftY+"  minRightX "+minRightX)
      if( maxLeftX <= minRightY && maxLeftY <= minRightX ) {

        if((x+y)%2 == 0 ) {
          println("scala.math.max(maxLeftX,maxLeftY) ===="+scala.math.max(maxLeftX,maxLeftY))
          println("scala.math.min(minRightX,minRightY) ===="+scala.math.min(minRightX,minRightY))
          result = (scala.math.max(maxLeftX,maxLeftY) + scala.math.min(minRightX,minRightY)).toDouble/2
          return result
        }
        else {
          result = scala.math.max(maxLeftX,maxLeftY)
          return result

        }
      }
      else if (maxLeftX > minRightY) {
        high = partitionX -1
      }
      else {
        low = partitionX+1
      }
    }
    return result
  }

  def longestPalindrome(str : String) : String ={
    var returnStr = ""
    var start : Int = 0
    var end : Int = 0
    var loop :Int = 0
    var len1 : Int = 0
    var len2 : Int = 0
    var len : Int = 0
    if (str == null || str.length < 1 ) return returnStr
    while ( loop < str.length) {
      len1 = expandMiddle(str, loop,loop)
      len2 = expandMiddle(str, loop,loop+1)
      len = Math.max(len1, len2)
      if(len > end - start) {
        start = loop - ((len-1)/2)
        end = loop +(len/2)
      }
      loop = loop +1
    }
    returnStr = str.substring(start,end +1)
    return returnStr
  }

  def expandMiddle( str: String, start :Int , end : Int):Int ={
    var left = start
    var right = end
    if (str == null || left > right) return 0
    while(left >= 0 && right < str.length && str.charAt(left) == str.charAt(right))
      {
        left = left -1
        right = right + 1
      }
    return right - left -1
  }


  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    var head = new ListNode(0)
    var result = head

    var current_sum =0
    var carry = 0
    var p1 = l1
    var p2 = l2

    while (p1 != null || p2 != null ){

      var v1 = 0
      var v2 = 0

      if(p1 != null) v1 = p1.x else v1 = 0
      if(p2 != null) v2 = p2.x else v2 = 0

      current_sum = v1 + v2 +carry
      carry = current_sum / 10

      var new_node = new ListNode(current_sum % 10)
      result.next = new_node
      result = result.next

      if(p1 != null) p1 = p1.next
      if(p2 != null) p2 = p2.next

    }
    if (carry > 0 ) {
      result.next = new ListNode(carry)
    }
    return head.next

  }
}
