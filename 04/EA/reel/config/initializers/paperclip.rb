Paperclip.interpolates :initials do |attachment, style|
  attachment.instance.name_initials
end

Paperclip.interpolates :dimensions do |attachment, style|
  {thumb: "100x100", medium: "300x300"}[style]
end
