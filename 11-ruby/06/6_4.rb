require 'csv'
total = 0
CSV.foreach("sales.csv", headers: true) { |row| total += row["amount"].to_f }
puts "Total sales: #{total}"
