require 'rails_helper'

RSpec.describe ReviewsController, type: :controller do
  login_user

  describe "POST #create" do
    let(:review_params) { FactoryGirl.attributes_for :review }
    before(:each) { @movie = FactoryGirl.create :movie }

    it "creates a review" do
      expect do
        post :create, movie_id: @movie.id, review: review_params
      end.to change(Review, :count).by(1)
    end

    it "associates the review to the current user" do
      post :create, movie_id: @movie.id, review: review_params
      expect(subject.current_user.reviewed_movies).to include(@movie)
    end
  end
end
