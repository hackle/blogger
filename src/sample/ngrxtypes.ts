type ActionTypes = 'UpdateBirthday' | 'UpdateName';

type UpdateBirthdayAction = {
    type: ActionTypes;
    payload: Date;
}

type UpdateNameAction = {
    type: ActionTypes;
    payload: string;
}

type State = {
  name: string,
  birthday: Date
}

type ActionUnion = UpdateBirthdayAction | UpdateNameAction;

type ActionBase = { type: ActionTypes }

function reducer1(state: State, action: ActionUnion): State {
    switch (action.type) {
        case 'UpdateBirthday': return { ...state, birthday: action.payload };
        case 'UpdateName': return { ...state, name: action.payload };
        default: return state;
    }
}