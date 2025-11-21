Dir.glob('*.txt').each_with_index do |file, index|
  new_name = "file_#{index + 1}.txt"
  File.rename(file, new_name)
end

puts "Renaming completed!"
