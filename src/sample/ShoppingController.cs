public class ShoppingController : BaseController
{
    public ShoppingController(ILoggingService loggingService, IStockService stockService)
    {
        // ...
    }

    public decimal CalculateTotalPayable(IEnumerable<Item> items, Membership member, string promoCode) 
    {
        var memberDiscountPercentage = 0;
        switch (member.Type)
        {
            case MemberType.Diamond: 
                memberDiscountPercentage = 10;
                break;
            case MemberType.Gold:
                memberDiscountPercentage = 5;
                break;
            default: 
                break;
        }

        var promoDiscountPercentage = 0;
        if (promoCode == "akaramba")
        {
            promoDiscountPercentage = 8;
        }
        else if (promoCode == "excellent")
        {
            promoDiscountPercentage = 6;
        }

        var discountToApply = Math.Max(memberDiscountPercentage, promoDiscountPercentage);
        var totalPayable = items.Sum(item => {
            if (item.IsDiscountable)
                return item.Price * (1.0 - discountToApply / 100);
            else 
                return item.Price;
        });

        this.loggingService.Log(LogLevel.Info, $"logging info");

        return totalPayable;
    }
}