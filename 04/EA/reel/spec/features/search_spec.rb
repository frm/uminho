require 'rails_helper'

feature 'Search' do
  background do
    user = FactoryGirl.create(:user)
    login_as(user, :scope => :user)
    @user = FactoryGirl.create(:user)
  end

  scenario 'search for user' do
    visit root_path
    fill_in 'Search', with: @user.email
    first('.btn').click
    expect(page).to have_link(@user.name, href: user_path(@user))
  end
end
