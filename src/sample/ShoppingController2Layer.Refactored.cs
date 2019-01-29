public class UserAccountController 
{
    [HttpPost]
    public void ChangePassword(ChangePasswordRequest request)
    {
        var userAccountRepository = new UserAccountRepository();
        ChangePassword(request, 
            this.User.Id,
            userAccountRepository.GetById,
            userAccountRepository.SetPassword);
    }

    public static void ChangePassword(ChangePasswordRequest request,
        int userId,
        Func<int, UserAccount> getUserAccountById,
        Action<string, int> setPasswordForUserId)
    {
        UserAccountService.ChangePassword(request.OldPassword, 
            request.NewPassword, 
            userId,
            getUserAccountById,
            setPasswordForUserId);
    }
}

public class UserAccountService
{
    public static void ChangePassword(string oldPassword, 
        string newPassword, 
        int userId,
        Func<int, UserAccount> getUserAccountById,
        Action<string> setPasswordForUserId) 
    {
        var userAccount = getUserAccountById(userId);

        // any validator can throw exception if validation fails
        UserAccountValidator.IsEnabled(userAccount);
        UserAccountValidator.PasswordMatches(userAccount, oldPassword);
        PasswordValidator.IsStrongEnough(newPassword);

        setPasswordForUserId(newPassword, userId);
    }
}

public class UserAccountRepository
{
    public UserAccount GetById(int userId)
    {
        // makes call to database
    }

    public void SetPassword(string password, int userId)
    {
        // similarly, makes call to database
    }
}