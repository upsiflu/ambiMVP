module Test.Generated.Main2832293454 exposing (main)

import VerifyExamples.Compositron.Structure.EagerZipperTree.Find0
import VerifyExamples.Compositron.Structure.EagerZipperTree.ModuleDoc0
import VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template2
import VerifyExamples.Compositron.Structure.EagerZipperTree.Group0
import VerifyExamples.Compositron.Structure.EagerZipperTree.Tree0
import VerifyExamples.Compositron.Structure.EagerZipperTree.Find2
import VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template0
import VerifyExamples.Compositron.Structure.EagerZipperTree.Find1
import VerifyExamples.Compositron.Structure.EagerZipperTree.Mark0
import VerifyExamples.Compositron.Structure.EagerZipperTree.Group1
import VerifyExamples.Compositron.Structure.EagerZipperTree.ModuleDoc1
import VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template1
import VerifyExamples.Compositron.Structure.EagerZipperTree.Branch0

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Find0" [VerifyExamples.Compositron.Structure.EagerZipperTree.Find0.spec0],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.ModuleDoc0" [VerifyExamples.Compositron.Structure.EagerZipperTree.ModuleDoc0.spec0],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template2" [VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template2.spec2],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Group0" [VerifyExamples.Compositron.Structure.EagerZipperTree.Group0.spec0],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Tree0" [VerifyExamples.Compositron.Structure.EagerZipperTree.Tree0.spec0],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Find2" [VerifyExamples.Compositron.Structure.EagerZipperTree.Find2.spec2],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template0" [VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template0.spec0],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Find1" [VerifyExamples.Compositron.Structure.EagerZipperTree.Find1.spec1],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Mark0" [VerifyExamples.Compositron.Structure.EagerZipperTree.Mark0.spec0],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Group1" [VerifyExamples.Compositron.Structure.EagerZipperTree.Group1.spec1],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.ModuleDoc1" [VerifyExamples.Compositron.Structure.EagerZipperTree.ModuleDoc1.spec1],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template1" [VerifyExamples.Compositron.Structure.EagerZipperTree.Accept_template1.spec1],     Test.describe "VerifyExamples.Compositron.Structure.EagerZipperTree.Branch0" [VerifyExamples.Compositron.Structure.EagerZipperTree.Branch0.spec0] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 349079895944772, processes = 4, paths = ["/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Accept_template0.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Accept_template1.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Accept_template2.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Branch0.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Find0.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Find1.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Find2.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Group0.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Group1.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Mark0.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/ModuleDoc0.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/ModuleDoc1.elm","/home/f/ambiMVP/tests/VerifyExamples/Compositron/Structure/EagerZipperTree/Tree0.elm"]}