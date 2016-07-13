FactoryGirl.define do
  factory :user do
    name { Faker::Name.name }
    email { Faker::Internet.safe_email(name) }
    bio { Faker::Lorem.paragraph(3) }
    password "foobarbaz"
    password_confirmation "foobarbaz"
  end
end
