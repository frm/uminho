FactoryGirl.define do
  factory :actor do
    name { Faker::Name.name }
    place_of_birth { Faker::Address.city }
    birthday { Faker::Date.between(80.years.ago, Date.today) }
    homepage { Faker::Internet.url }
    biography { Faker::Lorem.paragraph(3) }
  end
end
