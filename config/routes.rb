Rails.application.routes.draw do
  devise_for :users
  root 'welcome#index'

  resources :users, only: [:index, :show] do
    get :following, :followers
  end

  resources :movies, only: [:index, :show] do
    resources :reviews, only: [:create, :destroy]
  end

  resources :actors, only: [:show]

  resources :genres, only: [:index, :show]

  resources :relationships, only: [:create, :destroy]
end
