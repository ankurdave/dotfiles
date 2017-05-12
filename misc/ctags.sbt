val ctagsTask = TaskKey[Unit]("ctags", "Builds Scala tags")

ctagsTask := {
  import sys.process._
  baseDirectory(_ / ".tags").value.mkdir()
  val ret = try {
    Seq(
      "sctags", "-e",
      "-f", baseDirectory(_ / ".tags/project.tags").value.getAbsolutePath,
      "-R", baseDirectory.value.getAbsolutePath).!
  } catch {
    case e: Exception =>
      sLog.value.warn(e.toString)
      1
  }
  if (ret != 0) sLog.value.warn("Tag generation failed.")
}

baseDirectory in ctagsTask := (baseDirectory in ThisBuild).value

compile in Compile := { (compile in Compile).dependsOn(ctagsTask).value }
