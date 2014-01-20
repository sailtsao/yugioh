defmodule Yugioh.Data.Strings do
  def get(:login_again_string) do
    "<font color='#ffffff' size='14'>账号在其他地方登录,您已被迫下线</font>"
  end
  def get(_) do
    nil
  end
end