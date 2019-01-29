public class UserAccountController 
{
    private readonly IUserAccountService userAccountService;

    public UserAccountController(IUserAccountService userAccountService)
    {
        this.userAccountService = userAccountService;
    }

    [HttpPost]
    public void ChangePassword(ChangePasswordRequest request)
    {
        this.userAccountService.ChangePassword(request.OldPassword, request.NewPassword, this.UserId);
    }
}

public class UserAccountService : IUserAccountService
{
    private readonly IUserAccountRepository userAccountRepository;
    private readonly IUserAccountValidator userAccountValidator;
    private readonly IPasswordValidator passwordValidator;

    public UserAccountService(IUserAccountRepository userAccountRepository,
        IUserAccountValidator userAccountValidator,
        IPasswordValidator passwordValidator)
    {
        this.userAccountRepository = userAccountRepository;
        this.userAccountValidator = userAccountValidator;
        this.passwordValidator = passwordValidator;
    }
    public void ChangePassword(string oldPassword, string newPassword, int userId) 
    {
        var userAccount = this.userAccountRepository.GetById(userId);

        // any validator can throw exception if validation fails
        this.userAccountValidator.IsEnabled(userAccount);
        this.userAccountValidator.PasswordMatches(userAccount, oldPassword);
        this.passwordValidator.IsStrongEnough(newPassword);

        this.userAccountRepository.SetPassword(newPassword, userId);
    }
}

public class UserAccountRepository : IUserAccountRepository
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