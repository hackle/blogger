public class ShoppingController : BaseController
{
    public ShoppingController(ILoggingService loggingService, IStockService stockService)
    {
        // ...
    }

    public decimal CalculateTotalPayable(IEnumerable<Item> items, Membership member, string promoCode) 
    {
        var memberDiscountPercentage = MemberDiscount.GetInPercentage(member);
        var promoDiscountPercentage = PromoCodeDiscount.GetInPercentage(promoCode);

        var totalPayable = items.Sum(item => ItemSalePrice.Calculate(item, memberDiscountPercentage, promoDiscountPercentage));

        this.loggingService.Log(LogLevel.Info, $"logging info");

        return totalPayable;
    }
}

public static class MemberDiscount 
{
    public static int GetInPercentage(Membership member)
    {
        switch (member.Type)
        {
            case MemberType.Diamond: return 10;
            case MemberType.Gold: return 5;
            default: return 0;
        }
    }
}

public static class PromoCodeDiscount
{
    public static int GetInPercentage(string promoCode)
    {        
        if (promoCode == "akaramba")
        {
            return 8;
        }
        else if (promoCode == "excellent")
        {
            return 6;
        }

        return 0;
    }
}

public static class ItemSalePrice
{
    public static decimal Calculate(
        Item item, 
        decimal memberDiscountPercentage, 
        decimal promoDiscountPercentage)
    {
        var discountToApply = Math.Max(memberDiscountPercentage, promoDiscountPercentage);
        
        if (item.IsDiscountable)
            return item.Price * (1.0 - discountToApply / 100);
        else 
            return item.Price;
    }
}