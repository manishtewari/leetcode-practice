package scalaCourse

import scala.util.control.Breaks.break

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
    println("longest common prefix being ======"+longestCommonPrefix(Array("flower","flown","beautiful")))
  println("sum of two lists == "+addTwoNumbers(a,b).x)
    println("roman to integer == "+romanToInt("ZXVX"))
  println("longest palindrome === "+longestPalindrome("abcbaxyzyxab"))

  }
  /*To get the median of sorted arrays the time complexity */
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
      val maxLeftX = if (partitionX == 0) Integer.MIN_VALUE else nums1(partitionX -1 )
      val minRightX = if (partitionX == x) Integer.MAX_VALUE else nums1(partitionX)
      val maxLeftY = if (partitionY == 0) Integer.MIN_VALUE else nums2(partitionY -1 )
      val minRightY = if (partitionY == y) Integer.MAX_VALUE else nums2(partitionY)
      if( maxLeftX <= minRightY && maxLeftY <= minRightX ) {

        if((x+y)%2 == 0 ) {
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
  /*Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.*/
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    var start : Int =0
    var end : Int =nums.length-1
    var mid : Int = if (end < 2) 0 else if (end % 2 == 0) end/2 else (end-1)/2

    val retArr :Array[Int] = new Array(2)
    for(primary <- start to end )
    {
      val req = target-nums(primary)
      if(nums(end) == req) {
        retArr(0)= primary
        retArr(1)= end
        return retArr
      }
      for (sec <- primary.+(1) to end)
      {
        if(nums(sec) == req) {
          retArr(0)= primary
          retArr(1)= sec
          return retArr
        }
      }
    }
    return retArr
  }

/*Given a string s, find the length of the longest substring without repeating characters.*/
  def lengthOfLongestSubstring(s: String): Int = {
    var start = 0
    var end = 0
    var len = s.length()-1
    var longest = 0
    val mapInt : Set[Char]  = Set()

    while (end <= len)
    {
      var c = s.charAt(end)

      while(mapInt contains c ){
        mapInt.-(s.charAt(start))
        start = start +1
      }
      mapInt.+(s.charAt(end))
      longest = scala.math.max(longest,end-start+1)
      end = end + 1

    }
    return longest
  }

  /*Given an integer x, return true if x is palindrome integer.*/
  def isPalindrome(x: Int): Boolean = {
    var out =0
    var fetch = 0
    var modNum = x
    var Palindrome =false

    if (modNum == 0) Palindrome = true

    while (modNum > 0)
    {
      fetch = modNum%10
      modNum=modNum/10
      out = out*10+fetch
      if (out == x)
      {
        Palindrome = true
      }

    }
    return Palindrome
  }

  def romanToInt(s: String): Int = {

    var res = 0
    var length = s.length()
    var loop = length-1
    while(loop >= 0)
    {
      var out = s.charAt(loop) match {
        case 'V' => 5
        case 'L' => 50
        case 'D' => 500
        case 'M' => 1000
        case 'I' => 1
        case 'X' => 10
        case 'C' => 100
        case _ => 0
      }
      if (out == 0) {
        println("Not A Roman Value")
        break
      }
      res = res+out
      if(s.charAt(loop) == 'I' && loop < (length-1)){
        if(s.charAt(loop+1)=='V' || s.charAt(loop+1)=='X') res-=2;
      }
      if(s.charAt(loop) == 'X'  && loop < (length-1)){
        if(s.charAt(loop+1)=='L' || s.charAt(loop+1)=='C') res-=20;
      }
      if(s.charAt(loop) == 'C'  && loop < (length-1)){
        if(s.charAt(loop+1)=='D' || s.charAt(loop+1)=='M') res-=200;
      }
      loop = loop-1
    }
    return res
  }
  /*Initializes the process*/
  def longestCommonPrefix(strs : Array[String]):String ={
    if (strs == null || strs.length ==0) return ""
    return longestCommon(strs,0,strs.length-1)
  }
  /*This function distributes the list of string & tries to find the common prefix*/
  def longestCommon(strs: Array[String], left : Int, right: Int): String= {
    if (left == right){
      return strs(left)
    }
    else {
      var mid : Int = (left+right)/2
      var lcpLeft = longestCommon(strs,left,mid)
      var lcpRight = longestCommon(strs,mid+1,right)
      return commonPrefix(lcpLeft,lcpRight)
    }
  }
  /*The common prefix captures the common substring value starting from 0th index*/
  def commonPrefix(left :String, right: String): String = {
    var min = Math.min(left.length(), right.length())
    var i : Int = 0
    while (i < min)
    {
      if(left.charAt(i) != right.charAt(i))
      {
        return left.substring(0,i)
      }
      i= i+1
    }
    return left.substring(0,min)
  }
    def reverse(x: Int): Int = {
    var isNegative = x < 0
    var mod : Int = abs(x)
    var ret: Int = 0
    var result : Long =0
    def reverseIt (x: Int, rev : Long): Long = {
      if (x != 0) {
        result = rev * 10 + x % 10
        if (result > scala.Int.MaxValue || result < scala.Int.MinValue) result= 0
        reverseIt(x / 10,result)
      }
      result
    }
    var out = reverseIt(mod,ret).toInt
    if (isNegative) out = 0-out

    return out
  }
}
