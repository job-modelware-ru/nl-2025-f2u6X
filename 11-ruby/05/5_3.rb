require 'httparty'
require 'faker'
require 'json'

# Get users from API
response = HTTParty.get("https://jsonplaceholder.typicode.com/users")
users = JSON.parse(response.body)

# Random user from API
api_user = users.sample
puts "From API: #{api_user["name"]}, email: #{api_user["email"]}"

# Random fake user
fake_user = { name: Faker::Name.name, email: Faker::Internet.email }
puts "Fake user: #{fake_user[:name]}, email: #{fake_user[:email]}"
