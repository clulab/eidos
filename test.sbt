import Tests._

Test / fork := true // Also forces sequential operation

Test / parallelExecution := false // Keeps groups in their order

//Test / testForkedParallel := true // Allow parallel within group?

{
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

  testGrouping in Test := groupByLanguage((definedTests in Test).value)
}
