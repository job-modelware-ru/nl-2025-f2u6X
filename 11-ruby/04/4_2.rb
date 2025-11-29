require 'csv'

data = [
  {name: "Alice", age: 25},
  {name: "Bob", age: 30}
]

CSV.open("report.csv", "w") do |csv|
  csv << ["Name", "Age"]
  data.each { |row| csv << [row[:name], row[:age]] }
end
