items = [
  {name: "Milk", price: 50},
  {name: "Bread", price: 30},
  {name: "Cheese", price: 200}
]

expensive = items.select { |i| i[:price] > 80 }
puts "Expensive items: #{expensive.map { |i| i[:name] }}"

total = items.sum { |i| i[:price] }
puts "Total: #{total} RUB"