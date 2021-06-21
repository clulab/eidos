import Tests._

def groupByLanguage(tests: Seq[TestDefinition]) = {
  //def newRunPolicy = SubProcess(ForkOptions())
  def newRunPolicy = InProcess

  val groupedTestDefinitions: Map[String, Seq[TestDefinition]] = tests.groupBy { testDefinition =>
    if (testDefinition.name.contains(".text.english."))
      "englishGroup"
    else if (testDefinition.name.contains(".text.englishGrounding."))
      "englishGroundingGroup"
    else if (testDefinition.name.contains(".text.portuguese."))
      "portugueseGroup"
    else
      "other"
  }
  val groups: Seq[Group] = groupedTestDefinitions.toSeq.map { case (name, testDefinitions) =>
    new Group(name, testDefinitions, newRunPolicy)
  }

  groups
}

ThisBuild / Test / fork := true // also forces sequential operation
ThisBuild / Test / parallelExecution := false // keeps groups in their order
//ThisBuild / Test / testForkedParallel := true // Allow parallel within group?
ThisBuild / Test / testGrouping := groupByLanguage((Test / definedTests).value)
