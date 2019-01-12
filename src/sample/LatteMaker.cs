public interface IICoffeeBeanProvider
{
    CoffeeBean GetInGram(int grams);
}

public class CoffeeBeanProvider : IICoffeeBeanProvider 
{ 
    public CoffeeBean GetInGram(int grams)
    {
        // ...
    }
}

public interface IMilkProvider
{
    public Milk GetInOunce(int ounces);
}

public class MilkProvider : IMilkProvider
{
    public Milk GetInOunce(int ounces)
    {
        // ...
    }
}

public interface ILatteMaker
{
    Latte Make();
}

public class LatteMaker : ILatteMaker 
{
    private readonly ICoffeeBeanProvider beanProvider;
    private readonly IMilkProvider milkProvider;

    public LatteMaker(ICoffeeBeanProvider beanProvider, IMilkProvider milkProvider)
    {
        this.beanProvider = beanProvider;
        this.milkProvider = milkProvider;
    }

    public Latte Make() 
    {
        var beans = this.beanProvider.GetInGram(10);
        var milk = this.milkProvider.GetInOunce(8);

        return new Latte(this.Grind(beans), this.Steam(milk));
    }

    // Grinding and Steaming algorithms not included
}