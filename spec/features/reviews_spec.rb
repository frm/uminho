require 'rails_helper'

feature 'Creating a review' do
  background do
    @current_user = FactoryGirl.create(:user)
    login_as(@current_user, :scope => :user)
  end

  scenario 'reviewing a movie' do
    visit genres_path
    first('.overbox a').click
    click_on "Review"
    fill_in 'review_score', with: 4.0
    fill_in 'review_description', with: 'Good'
    click_on 'Create Review'
    expect(first('.review-title').text).to eq(@current_user.name)
    expect(first('.review-description').text).to eq('Good')
    expect(first('#score').value).to eq('4.0')
  end
end

