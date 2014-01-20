defmodule Yugioh.Data.Strings do
  def get(:login_again_string) do
    "账号在其他地方登录,您已被迫下线"
  end
  def get(_) do
    nil
  end
end