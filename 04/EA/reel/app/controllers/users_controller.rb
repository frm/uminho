class UsersController < ApplicationController
  before_action :authenticate_user!

  def index
    @users = User.all
  end

  def show
    @user = User.find(params[:id])
  end

  def following
    @users = User.find(params[:user_id]).following
    render :index
  end

  def followers
    @users = User.find(params[:user_id]).followers
    render :index
  end
end
