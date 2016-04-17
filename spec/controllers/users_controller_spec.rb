require 'rails_helper'

RSpec.describe UsersController, type: :controller do

  describe "GET #index" do
    it "assigns @users" do
      user_list = []
      2.times { user_list << FactoryGirl.create(:user) }
      get :index
      expect(assigns(:users)).to eq(user_list)
    end

    it "renders the index template" do
      get :index
      expect(response).to render_template("index")
    end
  end

  describe "GET #show" do
    before { @new_user = FactoryGirl.create :user }

    it "renders the show template" do
      get :show, id: @new_user.id
      expect(response).to render_template("show")
    end

    it "assigns the correct @user" do
      get :show, id: @new_user.id
      expect(assigns(:user)).to eq(@new_user)
    end
  end
end
