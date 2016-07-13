require 'rails_helper'

RSpec.describe RelationshipsController, type: :controller do
  login_user

  describe "POST #create" do
    context "with valid attributes" do
      before(:each) { @user = FactoryGirl.create :user }

      it "follows a user" do
        expect { post :create, followed_id: @user.id  }.to change(Relationship, :count).by(1)
      end

      it "adds the followed user to the followers" do
        post :create, followed_id: @user.id
        expect(subject.current_user.following).to include(@user)
      end
    end
  end

  describe "DELETE #destroy" do
    context "destroying an existing relationship" do
      before(:each) do
        @relationship = subject.current_user.follow FactoryGirl.create(:user)
      end

      it "decrements the total number of relationships" do
        expect { delete :destroy, id: @relationship.id }.to change(Relationship, :count).by(-1)
      end

      it "removes the unfollowed user from the followers list" do
        other_user = @relationship.followed
        delete :destroy, id: @relationship.id
        expect(subject.current_user.following).not_to include(other_user)
      end
    end
  end
end
