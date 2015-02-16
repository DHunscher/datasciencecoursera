# tests

test.pollutantmean.args <- function(
      directory = "/Users/hunsched/Box Sync/Coursera-data-science/datasciencecoursera/R_Programming/specdata",
      invalid.dir = "/#@$%$#@#$",
      valid.fn = "C:/Windows/win.ini") {
  print("First, a bunch of error messages checking arguments.")
  try(pollutantmean(23,"nitrate"))
  try(pollutantmean(invalid.dir,"nitrate"))
  try(pollutantmean(valid.fn,"nitrate"))
  try(pollutantmean(directory,"nutrate"))
  try(pollutantmean(directory,"nitrate","bob"))
  try(pollutantmean(directory,"nitrate",0:332))
  try(pollutantmean(directory,"nitrate",1:333))
  print("All that should follow now is a vector of TRUE's.")
  c(try(all.equal(pollutantmean(directory,"sulfate",1:2),4.402199,tolerance = 1.5e-7))
    ,try(all.equal(pollutantmean(directory,"sulfate",c(2,1)),4.402199,tolerance = 1.5e-7))
    ,try(all.equal(pollutantmean(directory,"nitrate",296:296),0.9135714,tolerance = 1.5e-7))
  )
}
