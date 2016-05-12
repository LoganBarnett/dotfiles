#!/usr/bin/ruby

require 'rubygems'
require 'curb'
require 'URI'
require 'json'
require 'ruby-growl'

# username           = "atlantisremixed"
# password           = "questforever"
gist_post_url      = "https://api.github.com/gists"
clipboard_contents = IO.popen( "pbpaste", "r+" ).read
filename           = ARGV[0] || "gist"

gist_data                                = {}
gist_data[:public]                     ||= false
gist_data[:files]                      ||= {}
gist_data[:files][filename]            ||= {}
gist_data[:files][filename][:content]  ||= {}
gist_data[:files][filename][:content]    = clipboard_contents

c = Curl::Easy.http_post( gist_post_url, gist_data.to_json ) do |curl|
  curl.headers["content-type"] = "application/json"
  # curl.http_auth_types         = :basic
  # curl.username                = username
  # curl.password                = password
end

result = JSON.parse( c.body_str )
url    = result["html_url"]

IO.popen( "pbcopy", "r+" ).puts url

g = Growl.new "localhost", "ruby-growl", ["ruby-growl Notification"]
g.notify "ruby-growl Notification", "Gist Created", url