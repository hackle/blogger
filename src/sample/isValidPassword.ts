function isValidPassword(password: string): boolean {
    if (null == password) 
        return false;
    
    return password.length >= 4 && password.length <= 8;
}

isValidPassword(123);