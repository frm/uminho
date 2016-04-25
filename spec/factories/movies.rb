FactoryGirl.define do
  factory :movie do
    sequence :id
    title { Faker::Book.title }
    year  { Faker::Date.between(80.years.ago, Date.today) }
  end
end
