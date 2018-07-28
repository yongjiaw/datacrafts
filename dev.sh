sbt# run this locally and clear any errors before making PR. The CI should be setup to run the same tasks

sbt "++ 2.12.6" clean coverage scalastyle test:scalastyle test coverageReport
