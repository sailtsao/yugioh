defmodule DBUtil do
  import Ecto.Query
  @doc """
  iex> DBUtil.role_exist?("sail")
  true
  """
  def role_exist?(name) do
    query = from(r in Model.Role,where: r.name == ^name,select: count(r.id))
    case Repo.all(query) do
      [0] ->
        false
      _ ->
        true
    end
  end  

  def get_roles_list(user_id) do
    Repo.all(from(r in Model.Role,where: r.user_id == ^user_id,select: {r.id,r.name,r.avatar}))
  end
  
  def get_role_data(role_id) do
    Repo.get(Model.Role,role_id)
  end
  
end