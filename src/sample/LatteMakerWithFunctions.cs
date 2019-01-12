
public class CoffeeBeanProvider 
{ 
    public CoffeeBean GetInGram(int grams)
    {
        // ...
    }
}

public class MilkProvider
{
    public CoffeeBean GetInOunce(int ounces)
    {
        // ...
    }
}

public static class LatteMaker 
{
    public static Latte Make(Func<int, CoffeeBean> getBeansInGram, Func<int, Milk> getMilkInOunce) 
    {
        var beans = getBeansInGram(10);
        var milk = getMilkInOunce(8);

        return new Latte(Grind(beans), Steam(milk));
    }

    // Grinding and Steaming algorithms not included
}