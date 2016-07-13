FactoryGirl.define do
  factory :relationship do
    follower_id { User.order("RANDOM()").first }
    followed_id do
      r = User.order("RANDOM()").first until r && r != follower_id
      r
    end
  end
end
