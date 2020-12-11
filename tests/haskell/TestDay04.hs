module TestDay04 where

import Day04
import Test.HUnit

examplePart1 =
  unlines
    [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
      "byr:1937 iyr:2017 cid:147 hgt:183cm",
      "",
      "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
      "hcl:#cfa07d byr:1929",
      "",
      "hcl:#ae17e1 iyr:2013",
      "eyr:2024",
      "ecl:brn pid:760753108 byr:1931",
      "hgt:179cm",
      "",
      "hcl:#cfa07d eyr:2025 pid:166559648",
      "iyr:2011 ecl:brn hgt:59in"
    ]

invalidPassportsPart2 =
  unlines
    [ "eyr:1972 cid:100",
      "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
      "",
      "iyr:2019",
      "hcl:#602927 eyr:1967 hgt:170cm",
      "ecl:grn pid:012533040 byr:1946",
      "",
      "hcl:dab227 iyr:2012",
      "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
      "",
      "hgt:59cm ecl:zzz",
      "eyr:2038 hcl:74454a iyr:2023",
      "pid:3556412378 byr:2007"
    ]

validPassportsPart2 =
  unlines
    [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
      "hcl:#623a2f",
      "",
      "eyr:2029 ecl:blu cid:129 byr:1989",
      "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
      "",
      "hcl:#888785",
      "hgt:164cm byr:2001 iyr:2015 cid:88",
      "pid:545766238 ecl:hzl",
      "eyr:2022",
      "",
      "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
    ]

testPart1 :: Test
testPart1 = TestList [TestCase $ assertEqual "" 2 (solvePart1 examplePart1)]

testByr :: Test
testByr =
  TestList
    [ TestCase $ assertEqual "" True (checkField "byr" "2002"),
      TestCase $ assertEqual "" False (checkField "byr" "2003")
    ]

testHgt :: Test
testHgt =
  TestList
    [ TestCase $ assertEqual "" True (checkField "hgt" "60in"),
      TestCase $ assertEqual "" True (checkField "hgt" "190cm"),
      TestCase $ assertEqual "" False (checkField "hgt" "190in"),
      TestCase $ assertEqual "" False (checkField "hgt" "190")
    ]

testHcl :: Test
testHcl =
  TestList
    [ TestCase $ assertEqual "" True (checkField "hcl" "#123abc"),
      TestCase $ assertEqual "" False (checkField "hcl" "#123abz"),
      TestCase $ assertEqual "" False (checkField "hcl" "123abc")
    ]

testEcl :: Test
testEcl =
  TestList
    [ TestCase $ assertEqual "" True (checkField "ecl" "brn"),
      TestCase $ assertEqual "" False (checkField "ecl" "wat")
    ]

testPid :: Test
testPid =
  TestList
    [ TestCase $ assertEqual "" True (checkField "pid" "000000001"),
      TestCase $ assertEqual "" False (checkField "pid" "0123456789")
    ]

testPart2 :: Test
testPart2 =
  TestList
    [ TestCase $ assertEqual "" 4 (solvePart2 validPassportsPart2),
      TestCase $ assertEqual "" 0 (solvePart2 invalidPassportsPart2)
    ]

runTests :: IO Counts
runTests = runTestTT $ TestList [testPart1, testByr, testHgt, testHcl, testEcl, testPid, testPart2]