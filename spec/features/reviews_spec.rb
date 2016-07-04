require 'rails_helper'

feature 'Creating a review' do
  background do
    @current_user = FactoryGirl.create(:user)
    login_as(@current_user, :scope => :user)
  end

  scenario 'reviewing a movie' do
    visit genres_path
    first('.gallery-entry').first('a').click
    click_on "Review"
    fill_in 'review_score', with: 4.0
    fill_in 'review_description', with: 'Good'
    click_on 'Create Review'
    expect(first('.review-title').text).to eq(@current_user.name)
    expect(first('.review-description').text).to eq('Good')
    expect(first('#score').value).to eq('4.0')
  end

  #scenario 'reviewing a movie', js: true do
  #  visit genres_path
  #  first('.gallery-entry').hover
  #  first('.overbox').click
  #  click_on "Review"
  #  find('.star:nth-child(4)').click
  #  fill_in 'review_description', with: 'Good'
  #  click_on 'Create Review'
  #  expect(first('.review-title').text).to eq(@current_user.name)
  #  expect(first('.review-description').text).to eq('Good')
  #  expect(first('.badge').text).to eq('3.5')
  #end

end

