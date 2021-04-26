import Tests._

def groupByLanguage(tests: Seq[TestDefinition]) = {
  //def newRunPolicy = SubProcess(ForkOptions())
  def newRunPolicy = InProcess

  val englishTests = tests.filter(_.name.contains(".text.english."))
  val portugueseTests = tests.filter(_.name.contains(".text.portuguese."))
  val languageNames = englishTests.map(_.name) ++ portugueseTests.map(_.name)
  val otherTests = tests.filter(test => !languageNames.contains(test.name))
  val allNames = otherTests.map(_.name) ++ languageNames
//    val otherAndEnglishGroup = new Group("otherAndEnglish", otherTests ++ englishTests, newWubProcess) 
  val englishGroup = new Group("english", englishTests, newRunPolicy)
  val portugueseGroup = new Group("portuguese", portugueseTests, newRunPolicy)
  val otherGroup = new Group("other", otherTests, newRunPolicy)

  Seq(otherGroup, englishGroup, portugueseGroup)
}

ThisBuild / Test / fork := true // also forces sequential operation
ThisBuild / Test / parallelExecution := false // keeps groups in their order
//ThisBuild / Test / testForkedParallel := true // Allow parallel within group?
ThisBuild / Test / testGrouping := groupByLanguage((Test / definedTests).value)
