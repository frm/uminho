require 'rails_helper'

feature 'Voting' do
  background do
    user = FactoryGirl.create(:user)
    @current_user = FactoryGirl.create(:user)
    login_as(@current_user, :scope => :user)
    @review = FactoryGirl.create(:review, user_id: user.id, movie_id: Movie.trending.first.id)
  end

  scenario 'review upvoting' do
    visit movie_path(@review.movie)
    first('.review-vote.up').click
    expect(first('.review-reliability .badge').text).to eq('1')
  end

  scenario 'review downvoting' do
    visit movie_path(@review.movie)
    first('.review-vote.down').click
    expect(first('.review-reliability .badge').text).to eq('-1')
  end
end
