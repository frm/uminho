require 'rails_helper'

RSpec.describe Review, type: :model do
  before do
    @user = FactoryGirl.create :user
    @review = @user.reviews.build FactoryGirl.attributes_for(:review,
                                                             movie_id: 1)
  end

  subject { @review }

  describe "basic review" do
    it { should be_valid }
  end

  describe "required fields" do
    describe "when user is blank" do
      before { @review.user = nil }
      it { should_not be_valid }
    end

    describe "when movie is blank" do
      before { @review.movie = nil }
      it { should_not be_valid }
    end

    describe "when score is blank" do
      before { @review.score = nil }
      it { should_not be_valid }
    end

    describe "when description is blank" do
      before { @review.description = nil }
      it { should be_valid }
    end
  end

  describe "field limit" do
    describe "when description is too long" do
      before { @review.description = 'a' * 1001 }
      it { should_not be_valid }
    end
  end

  describe "score format" do
    describe "when score is below 0" do
      before { @review.score = -1.0 }
      it { should_not be_valid }
    end

    describe "when score is over 10" do
      before { @review.score = -1.0 }
      it { should_not be_valid }
    end

    describe "when format isn't in halves" do
      before { @review.score = 1.2 }
      it { should_not be_valid }
    end
  end

  it "should be limited to one user review per movie" do
    @review.save
    @other_review = @user.reviews.build FactoryGirl.attributes_for(
                                                      :review, movie_id: 1)

    expect(@other_review).not_to be_valid
  end
end
