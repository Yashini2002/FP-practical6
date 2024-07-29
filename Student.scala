import scala.io.StdIn.readLine

object Student{

    def main(args:Array[String]):Unit={
        val studentInfo = getStudentInfoWithRetry()
        printStudentRecord(studentInfo)
    }

    def getStudentInfo(): (String, Int, Int, Double, Char)={
         val name = readLine("Enter student's name: ")
         val marks = readLine("Enter marks obtained: ").toInt
         val totalMarks = readLine("Enter total possible marks: ").toInt

         val percentage = (marks.toDouble / totalMarks) * 100
         val grade = calculateGrade(percentage)

         (name, marks, totalMarks, percentage, grade)
    }

    def printStudentRecord(studentInfo: (String, Int, Int, Double, Char)): Unit = {
        val (name, marks, totalMarks, percentage, grade) = studentInfo
        println(s"Student Name: $name")
        println(s"Marks Obtained: $marks")
        println(s"Total Possible Marks: $totalMarks")
        println(f"Percentage: $percentage%.2f%%")
        println(s"Grade: $grade")
    }

     def validateInput(name: String, marksStr: String, totalMarksStr: String): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (!marksStr.forall(_.isDigit)) {
      (false, Some("Marks must be a positive integer"))
    } else if (!totalMarksStr.forall(_.isDigit)) {
      (false, Some("Total possible marks must be a positive integer"))
    } else {
      val marks = marksStr.toInt
      val totalMarks = totalMarksStr.toInt
      if (marks < 0 || marks > totalMarks) {
        (false, Some("Marks must be non-negative and less than or equal to total possible marks"))
      } else {
        (true, None)
      }
    }
  }

    def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
       var isValid = false
       var studentInfo: (String, Int, Int, Double, Char) = null

       while (!isValid) {
         val name = readLine("Enter student's name: ")
         val marksStr = readLine("Enter marks obtained: ")
         val totalMarksStr = readLine("Enter total possible marks: ")

         val validationResult = validateInput(name, marksStr, totalMarksStr)

         validationResult match {
          case (true, None) =>
            val marks = marksStr.toInt
            val totalMarks = totalMarksStr.toInt
            val percentage = (marks.toDouble / totalMarks) * 100
            val grade = calculateGrade(percentage)
            studentInfo = (name, marks, totalMarks, percentage, grade)
            isValid = true
          case (false, Some(errorMessage)) =>
              println(s"Invalid input: $errorMessage")
          case _ =>
              println("Unexpected error occurred")
         }
       }
       studentInfo
    }

    def calculateGrade(percentage: Double): Char = {
       if (percentage >= 90) 'A'
       else if (percentage >= 75) 'B'
       else if (percentage >= 50) 'C'
       else 'D'
    }

}


    
