USER_COUNT = 50

USER_COUNT.times do
  FactoryGirl.create :user
end

User.create!(
  name: "Fernando Mendes",
  email: "a@b.com",
  password: "12345678",
  password_confirmation: "12345678")
