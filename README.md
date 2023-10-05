# listzipper

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/co.blocke/listzipper_3/badge.svg)](https://central.sonatype.com/artifact/co.blocke/listzipper_3)
[![Continuous Integration](https://github.com/gzoller/listzipper/actions/workflows/ci.yml/badge.svg)](https://github.com/gzoller/listzipper/actions/workflows/ci.yml)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/74067698d2ac414b9a463450ed728481)](https://app.codacy.com/gh/gzoller/listzipper/dashboard?utm_source=gh&utm_medium=referral&utm_content=&utm_campaign=Badge_grade)
[![Coverage Status](https://coveralls.io/repos/github/gzoller/listzipper/badge.svg)](https://coveralls.io/github/gzoller/listzipper)

ListZipper is noth'n fancy.  It's a handy zipper type over List that let's you nav, modify, insert, delete, and merge along the zipper.

## Use

Include it in your projects by adding the following to your build.sbt:

    libraryDependencies ++= Seq("co.blocke" %% "listzipper" % "0.1.6")
