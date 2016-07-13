require 'rails_helper'

RSpec.describe User, type: :model do
  before do
    @user = FactoryGirl.build :user
  end

  subject { @user }

  describe "basic user" do
    it { should be_valid }
  end

  describe "required fields" do
    describe "when name is blank" do
      before { @user.name = nil }
      it { should_not be_valid }
    end

    describe "when bio is blank" do
      before { @user.bio = nil }
      it { should be_valid }
    end
  end

  describe "field limit" do
    describe "when name is too long" do
      before { @user.name = 'a' * 76 }
      it { should_not be_valid }
    end

    describe "when bio is too long" do
      before { @user.bio = 'a' * 301 }
      it { should_not be_valid }
    end
  end

  describe "relationships" do
    it "should follow and unfollow users" do
      first_user = FactoryGirl.create :user
      other_user = FactoryGirl.create :user
      expect(first_user.following? other_user).to be false

      first_user.follow(other_user)
      expect(first_user.following? other_user).to be true
      expect(other_user.followed_by? first_user).to be true

      first_user.unfollow(other_user)
      expect(first_user.following? other_user).to be false
      expect(other_user.followed_by? first_user).to be false
    end
  end
end
