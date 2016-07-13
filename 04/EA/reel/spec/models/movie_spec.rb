require 'rails_helper'

RSpec.describe Movie, type: :model do
  before do
    @movie = FactoryGirl.create :movie
  end

  it "calculates the correct review average" do
    @movie.reviews.create(
          FactoryGirl.attributes_for :review, user_id: 1, score: 5)
    @movie.reviews.create(
          FactoryGirl.attributes_for :review, user_id: 2, score: 3)

    expect(@movie.rating).to eq(4)
  end

end
