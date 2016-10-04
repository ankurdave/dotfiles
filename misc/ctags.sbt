val ctagsTask = TaskKey[Unit]("ctags", "Builds Scala tags")

ctagsTask := {
  import sys.process._
  baseDirectory(_ / ".tags").value.mkdir()
  val ret = Seq(
    "sctags", "-e",
    "-f", baseDirectory(_ / ".tags/project.tags").value.getAbsolutePath,
    "-R", baseDirectory.value.getAbsolutePath).!
  if (ret != 0) error("Tag generation failed.")
}

baseDirectory in ctagsTask := (baseDirectory in ThisBuild).value

compile in Compile <<= (compile in Compile).dependsOn(ctagsTask)
