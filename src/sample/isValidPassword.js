function isValidPassword1(password) {
    if (typeof password !== "string") 
        throw "Password must be a string";
    
    return password.length >= 4 && password.length <= 8;
}