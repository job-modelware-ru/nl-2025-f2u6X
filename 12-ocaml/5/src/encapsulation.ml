class bank_account initial_balance =
  object (self)
    val mutable balance = initial_balance
    val account_number = Random.int 10000

    method private validate_amount amount =
      amount > 0
end
