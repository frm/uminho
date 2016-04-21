FactoryGirl.define do
  factory :review do
    score { (1..10).step(0.5).to_a.sample(1).first }
    description { Faker::Lorem.paragraph(3) }
  end
end
