require 'rails_helper'

feature 'User Relationships' do
  background do
    @current_user = FactoryGirl.create(:user)
    login_as(@current_user, :scope => :user)
    @user = FactoryGirl.create(:user)
  end

  scenario 'follow user' do
    visit user_path(@user)
    click_on 'Follow'
    expect(page).to have_button('Unfollow')
    expect(page).to have_link("#{@user.followers.count}Followers",
                              href: user_followers_path(@user))
  end

  scenario 'unfollow user' do
    @current_user.follow(@user)
    visit user_path(@user)
    click_on 'Unfollow'
    expect(page).to have_button('Follow')
    expect(page).to have_link("#{@user.followers.count}Followers",
                             href: user_followers_path(@user))
  end
end
