Rails.application.routes.draw do
  devise_for :users
  root 'welcome#index'

  resources :users, only: [:index, :show] do
    get :following, :followers
  end

  resources :movies, only: [:index, :show] do
    resources :reviews, only: [:create, :destroy, :update]
  end

  resources :actors, only: [:show]

  resources :genres, only: [:index, :show] do
    get '/search/actor' => 'discover#actor', as: 'actor_search'
    get '/search/user' => 'discover#user', as: 'user_search'
  end

  resources :relationships, only: [:create, :destroy]

  get '/search' => 'search#index'

  scope '/reviews' do
    post '/:review_id/vote' => 'ratings#create', as: 'reviews_vote'
    delete '/:review_id/unvote' => 'ratings#destroy', as: 'reviews_unvote'
  end
end
