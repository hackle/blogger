function isValidPassword(password: string): boolean {
    if (null == password) 
        return false;
    
    return password.length >= 4 && password.length <= 8;
}

// isValidPassword(123);

function amHappy(dayOfWeek: 'Friday' | 'Saturday'): boolean {
    return true;
}

// amHappy('Monday');