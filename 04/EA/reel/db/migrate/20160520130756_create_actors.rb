class CreateActors < ActiveRecord::Migration
  def change
    create_table :actors do |t|
      t.string :name
      t.string :profile_path
      t.string :character
      t.string :place_of_birth
      t.string :homepage
      t.date :birthday
      t.text :biography

      t.timestamps null: false
    end
  end
end
